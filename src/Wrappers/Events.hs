-- | Abstract event wrapper
module Wrappers.Events ( Event (..)
                       , Button (..)
                       , GLFW.Key (..)
                       , GLFW.SpecialKey (..)
                       , GLFW.MouseButton (..)
                       , GLFW.KeyButtonState (..)
                       , EventConstraint (..)
                       , Size (..)
                       , ButtonState
                       , EventPoller
                       , createEventPoller
                       ) where

import Util.Prelewd

import Data.Either
import Text.Show

import Util.IO
import Util.Queue

import Wrappers.OpenGL
import Wrappers.STM

import qualified Graphics.UI.GLFW as GLFW

-- | The state of an input device button
type ButtonState = GLFW.KeyButtonState

-- | Event data structure dictates what events we can accept
data Event = ButtonEvent Button ButtonState
           | MouseMoveEvent Position
           | ResizeEvent Size
           | RefreshEvent
           | CloseEvent
    deriving (Eq, Show)

-- | Buttons can be keys or mouse presses
data Button = KeyButton GLFW.Key
            | MouseButton GLFW.MouseButton
    deriving (Eq, Show)

-- | Filters to use when getting events with `poll`
data EventConstraint = ButtonEvents (Maybe Button) (Maybe ButtonState)
                     | MouseMoveEvents
                     | ResizeEvents
                     | RefreshEvents
                     | CloseEvents
    deriving (Eq, Show)

-- | Poll for specific types of events
type EventPoller = [EventConstraint] -- ^ Event types to filter for
                 -> IO [Event]

-- | Push an event into the shared variable.
addEvent :: TVar (Queue Event) -> Event -> IO ()
addEvent es s = void $ atomically $ modifyTVar es $ enq s

-- | Set up a queued event system
-- `GLFW.initialize` must have been called
createEventPoller :: IO EventPoller
createEventPoller = atomically (newTVar mempty) >>= go
    where
        go events = poll events <$ do
            True <- GLFW.initialize
            GLFW.windowCloseCallback $= True <$ addEvent events CloseEvent
            GLFW.windowSizeCallback $= addEvent events . ResizeEvent
            GLFW.windowRefreshCallback $= addEvent events RefreshEvent
            GLFW.keyCallback $= \k -> addEvent events . ButtonEvent (KeyButton k)
            GLFW.mouseButtonCallback $= \b -> addEvent events . ButtonEvent (MouseButton b)
            GLFW.mousePosCallback $= addEvent events . MouseMoveEvent

-- | True if the maybe is Nothing, or the value it holds matches
matchesMaybe :: Eq a => a -> Maybe a -> Bool
matchesMaybe = maybe True . (==)

-- | Get a list of events matching any of the constraints, starting with the most recent
poll :: TVar (Queue Event) -> EventPoller
poll es constraints = atomically $ do
        (es', ret) <- partition constraintMatch . toList <$> readTVar es
        ret <$ writeTVar es (fromList es')
    where
        constraintMatch e = if (e `matchesConstraint`) `any` constraints
                            then Right e
                            else Left e

        matchesConstraint (ButtonEvent s b) (ButtonEvents ms mb) = matchesMaybe s ms && matchesMaybe b mb
        matchesConstraint (MouseMoveEvent _) MouseMoveEvents = True
        matchesConstraint (ResizeEvent _) ResizeEvents = True
        matchesConstraint CloseEvent CloseEvents = True
        matchesConstraint RefreshEvent RefreshEvents = True
        matchesConstraint _ _ = False
