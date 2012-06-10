module Wrappers.Events ( Event (..)
                       , Button (..)
                       , GLFW.Key (..)
                       , GLFW.MouseButton (..)
                       , EventConstraint (..)
                       , Size (..)
                       , ButtonState
                       , EventPoller
                       , createEventPoller
                       ) where

import Prelude ()
import Util.Prelewd

import Data.Either
import Text.Show

import Util.Impure
import Util.Queue

import Wrappers.OpenGL
import Wrappers.STM

import qualified Graphics.UI.GLFW as GLFW

data Event = ButtonEvent Button ButtonState
           | MouseMoveEvent Position
           | ResizeEvent Size
           | RefreshEvent
           | CloseEvent
    deriving (Eq, Show)

data Button = KeyButton GLFW.Key
            | MouseButton GLFW.MouseButton
    deriving (Eq, Show)

data EventConstraint = ButtonEvents (Maybe Button) (Maybe ButtonState)
                     | MouseMoveEvents
                     | ResizeEvents
                     | RefreshEvents
                     | CloseEvents
    deriving (Eq, Show)

type ButtonState = GLFW.KeyButtonState

type EventPoller = [EventConstraint] -- ^ Event types to filter for
                 -> IO [Event]
type Events = TVar (Queue Event)

-- Push an event into the shared variable.
addEvent :: Events -> Event -> IO ()
addEvent es s = void $ atomically $ modifyTVar es $ enq s

-- | Set up a queued event system.
-- `GLFW.initialize` must have been called.
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

-- True if the maybe is Nothing, or the value it holds matches.
matchesMaybe :: Eq a => a -> Maybe a -> Bool
matchesMaybe = maybe True . (==)

-- | Get a list of events matching any of the constraints.
poll :: Events -> EventPoller
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
