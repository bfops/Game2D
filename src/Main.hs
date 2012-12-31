{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import Impure
import IO

import Control.Arrow (first)
import Data.Tuple
import Storage.Map
import Subset.Num
import Template.MemberTransformer

import Config

import Game.Input
import Game.State
import Game.Update
import Physics.Types
import Util.Unit
import Wrappers.Events
import Wrappers.GLFW

import Main.Graphics

-- | Loop with an iterator.. forever
loop :: Monad m => (a -> m a) -> a -> m a
loop f x = f x >>= loop f

-- | Monad-level if
ifm :: MonadPlus m => m Bool -> m a -> m a
ifm b x = b >>= guard >> x

-- | Is the window open?
isOpen :: EventPoller -> IO Bool
isOpen poll = null <$> poll [CloseEvents]

-- | Program state
data State = State { game       :: GameState
                   , lastUpdate :: Double
                   , inputs     :: Map Input (Positive Integer, Time)
                   }

$(memberTransformers ''State)

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        poll <- createEventPoller
        quiet $ loop (mainLoop poll) =<< getInitState

-- | Create initial program state
getInitState :: IO State
getInitState = State initState <$> io getTime <&> ($ mempty)

mainLoop :: EventPoller -> State -> IO State
mainLoop poll state = ifm (isOpen poll)
                    $ updateGraphics poll (game state)
                    >> io getTime
                   >>= updateState poll state

-- | Advance the program state by one iteration.
updateState :: EventPoller
            -> State        -- ^ Initial state
            -> Double       -- ^ Elapsed time
            -> IO State
updateState poll state t = getInputs poll
                         <&> foldr updateInput (map Just <$> inputs state)
                         <&> newState state t 

-- | Update the input state with a new input event
updateInput :: (Input, ButtonState)
            -> Map Input (Positive Integer, Maybe Time)
            -> Map Input (Positive Integer, Maybe Time)
updateInput (i, Press) ins = insertWith (\_ -> first (+ 1)) i (1, Nothing) ins
updateInput (i, Release) ins = modify decCount i ins <?> error "Released unpressed input"
    where decCount (n, t) = toPos (fromPos n - 1) <&> (, t)

-- | Receive all pending input events, and convert them to game input
getInputs :: EventPoller -> IO [(Input, ButtonState)]
getInputs poll = mapMaybe rawToInput <$> poll [ ButtonEvents Nothing Nothing, MouseMoveEvents ]
    where
        -- Convert an input event to a game input
        rawToInput :: Event -> Maybe (Input, ButtonState)
        rawToInput (ButtonEvent (KeyButton key) s) = lookup key keymap <&> (, s)
        rawToInput _ = Nothing

-- | Update the program state
newState :: State                                       -- ^ Initial state
         -> Double                                      -- ^ Elapsed time
         -> Map Input (Positive Integer, Maybe Time)    -- ^ Each pressed input mapped to
                                                        --   Just (hold time) | Nothing if newly-pressed
         -> State                                       -- ^ New state
newState s t is = let deltaT = Unit $ realToFrac $ t - lastUpdate s
                  in (setMembers <$> toNat deltaT <?> error "Faster-than-light play not enabled in the free version.") s
    where
        -- Set State members with a time slice
        setMembers dt = game' (update (snd <$> is) dt)
                      . lastUpdate' (\_-> t)
                      . inputs' (\_-> map ((+ dt).(<?> 0)) <$> is)
