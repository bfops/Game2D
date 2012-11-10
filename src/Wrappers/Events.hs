-- | Abstract event wrapper
module Wrappers.Events ( Event (..)
                       , Button (..)
                       , GLFW.Key (..)
                       , GLFW.MouseButton (..)
                       , EventConstraint (..)
                       , Size (..)
                       , ButtonState (..)
                       , EventPoller
                       , createEventPoller
                       ) where

import Prelewd

import IO

import Data.Either
import Text.Show

import Storage.List
import Storage.Queue

import Wrappers.OpenGL
import Wrappers.STM

import qualified Graphics.UI.GLFW as GLFW

-- | The state of an input device button
data ButtonState = Press | Release
    deriving (Show, Eq)

btnState :: Bool -> ButtonState
btnState True = Press
btnState False = Release

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
addEvent = void . atomically .$ modifyTVar .^ enq

-- | Set up a queued event system
-- `GLFW.initialize` must have been called
createEventPoller :: IO EventPoller
createEventPoller = atomically (newTVar mempty) >>= go
    where
        go events = mfilter identity (io GLFW.initialize) >> io (setCallbacks events) $> poll events

        toSize = on Size fromIntegral
        toPos = on Position fromIntegral

        setCallbacks events = sequence_
            [ GLFW.setWindowCloseCallback $ True <$ (runIO $ addEvent events CloseEvent)
            , GLFW.setWindowSizeCallback $ runIO . addEvent events . ResizeEvent .$ toSize
            , GLFW.setWindowRefreshCallback $ runIO (addEvent events RefreshEvent)
            , GLFW.setKeyCallback $ runIO . addEvent events .$ ButtonEvent . KeyButton .^ btnState
            , GLFW.setMouseButtonCallback $ runIO . addEvent events .$ ButtonEvent . MouseButton .^ btnState
            , GLFW.setMousePositionCallback $ runIO . addEvent events . MouseMoveEvent .$ toPos
            ]

-- | True if the maybe is Nothing, or the value it holds matches
matchesMaybe :: Eq a => a -> Maybe a -> Bool
matchesMaybe = maybe True . (==)

-- | Get a list of events matching any of the constraints, starting with the most recent
poll :: TVar (Queue Event) -> EventPoller
poll es constraints = atomically $ do
        (es', ret) <- partition constraintMatch <$> toList <$> readTVar es
        ret <$ writeTVar es (fromList es')
    where
        constraintMatch e = if (`any` constraints) $ matchesConstraint e
                            then Right e
                            else Left e

        matchesConstraint (ButtonEvent s b) (ButtonEvents ms mb) = matchesMaybe s ms && matchesMaybe b mb
        matchesConstraint (MouseMoveEvent _) MouseMoveEvents = True
        matchesConstraint (ResizeEvent _) ResizeEvents = True
        matchesConstraint CloseEvent CloseEvents = True
        matchesConstraint RefreshEvent RefreshEvents = True
        matchesConstraint _ _ = False
