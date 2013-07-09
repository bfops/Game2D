{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd hiding (Either (..))

import Impure
import IO

import Control.Concurrent (threadDelay)
import Control.Stream
import Data.Tuple
import Storage.Map
import Subset.Num

import Game.Input
import Game.Update
import Physics.Types
import Util.Unit
import Wrappers.Events
import Wrappers.GLFW

import Main.Graphics

-- | Title of the game window
title :: Text
title = "Game"

-- | What controls what?
keymap :: Map Key Input
keymap = map2 CharKey $ fromList
       [ (' ', Jump)
       , ('W', Jump)
       , ('A', Left)
       , ('D', Right)
       , ('R', Reset)
       ]

-- | GLFW display options
displayOpts :: DisplayOptions
displayOpts = defaultDisplayOptions
    { displayOptions_width = 800
    , displayOptions_height = 600
    , displayOptions_windowIsResizable = False
    }

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        initEvents
        iterateM_ (map snd . ($< ())) $ elapsedTime &&& events
                                    >>> inputs &&& arr fst
                                    >>> identify game
                                    >>> updateGraphics
                                    >>> lift (arr $ \_-> io $ threadDelay 10000)

inputs :: Stream IO (Time, [Event]) (Map Input (Maybe Time))
inputs = map convertEvents >>> identify (updater (barr updatePushed) mempty) >>> arr (map snd)
    where
        updatePushed (t, ins) pushed = foldr input (Just . (t +) . (<?> 0) <$$> pushed) ins

convertEvents :: Stream IO [Event] [(Input, ButtonState)]
convertEvents = lift $ arr $ map (mapMaybe id) . sequence . map convertEvent
    where
        convertEvent CloseEvent = mzero
        convertEvent (ResizeEvent s) = resize s $> Nothing
        convertEvent (ButtonEvent (KeyButton key) s) = return $ lookup key keymap <&> (, s)
        convertEvent _ = return Nothing

elapsedTime :: Stream IO () Time
elapsedTime = lift (arr $ \_-> io getTime) >>> identify (loop (barr diffT) Nothing)
    where
        diffT t (Just t') = (toNat (Unit $ realToFrac $ t - t') <?> error "Negative time elapsed", Just t)
        diffT t Nothing = (0, Just t)

-- | Update the input state with a new input event
input :: (Input, ButtonState)
      -> Map Input (Positive Integer, Maybe Time)
      -> Map Input (Positive Integer, Maybe Time)
input (i, Press) ins = insertWith (\_ -> map2 (+ 1)) i (1, Nothing) ins
input (i, Release) ins = modify decCount i ins <?> error "Released unpressed input"
    where decCount (n, t) = toPos (fromPos n - 1) <&> (, t)
