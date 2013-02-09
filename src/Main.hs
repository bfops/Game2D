{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import Impure
import IO

import Control.Stream
import Data.Tuple
import Storage.Id
import Storage.Map
import Subset.Num

import Config

import Game.Input
import Game.Update
import Physics.Types
import Util.Unit
import Wrappers.Events
import Wrappers.GLFW

import Main.Graphics

cycle :: Monad m => a -> Stream m a a -> m a
cycle x s = (x >$ s) >>= uncurry cycle

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        initEvents
        cycle () $ elapsedTime &&& events
                 >>> inputs &&& arr fst
                 >>> identify game
                 >>> updateGraphics

inputs :: Stream IO (Time, [Event]) (Map Input (Maybe Time))
inputs = map convertEvents >>> identify (arr Just >>> updater (Id <$$> updatePushed) mempty) >>> arr (map snd)
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
elapsedTime = lift (arr $ \_-> io getTime) >>> identify (blackBox diffT Nothing)
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
