module Wrappers.SDL ( module Graphics.UI.SDL.Audio
                    , module Graphics.UI.SDL.CPUInfo
                    , module Graphics.UI.SDL.Color
                    , module Graphics.UI.SDL.General
                    , module Graphics.UI.SDL.Joystick
                    , module Graphics.UI.SDL.Keysym
                    , module Graphics.UI.SDL.RWOps
                    , module Graphics.UI.SDL.Rect
                    , module Graphics.UI.SDL.Time
                    , module Graphics.UI.SDL.Types
                    , module Graphics.UI.SDL.Utilities
                    , module Graphics.UI.SDL.Version
                    , module Graphics.UI.SDL.Video
                    , module Graphics.UI.SDL.WindowManagement
                    , Event (..)
                    , ButtonState (..)
                    , FocusState (..)
                    , FocusObject
                    , Int16
                    , toSDLEvent
                    , fromSDLEvent
                    , tryPushEvent
                    , pushEvent
                    , pollEvent
                    , waitEvent
                    , waitEventBlocking
                    , Events.SDLEvent
                    , Events.UserEventID
                    , Events.MouseButton
                    , Events.toSafePtr
                    , Events.tryFromSafePtr
                    , Events.fromSafePtr
                    , Events.typeOfSafePtr
                    , Events.enableKeyRepeat
                    , Events.enableUnicode
                    , Events.queryUnicodeState
                    , Events.getKeyName
                    , Events.getMouseState
                    , Events.getRelativeMouseState
                    , Events.getModState
                    , Events.setModState
                    , Events.pumpEvents
                    , Events.enableEvent
                    , Events.queryEventState
                    , Events.getAppState
                    ) where

import Data.Bool
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int
import Data.Maybe
import Data.Word
import System.IO
import Text.Show

import qualified Graphics.UI.SDL.Events as Events
import Graphics.UI.SDL.Audio
import Graphics.UI.SDL.CPUInfo
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Joystick
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.RWOps
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.Version
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.WindowManagement
import Graphics.Rendering.OpenGL as OGL

data Event = Focus FocusState [FocusObject]
           | Key ButtonState !Keysym
           | MouseMotion !(Vertex2 Word16) !(Vector2 Int16)
           | MouseButton !ButtonState !(Vertex2 Word16) !Events.MouseButton
           | Resize !Int !Int
           | Expose
           | Quit
           | Unknown
    deriving (Eq, Show)

data ButtonState = Pressed | Released deriving (Eq, Show)
data FocusState = Gained | Lost deriving (Eq, Show)

type FocusObject = Events.Focus

toSDLEvent :: Event -> Events.Event
toSDLEvent (Focus Gained l) = Events.GotFocus l
toSDLEvent (Focus Lost l) = Events.LostFocus l
toSDLEvent (Key Pressed k) = Events.KeyDown k
toSDLEvent (Key Released k) = Events.KeyUp k
toSDLEvent (MouseMotion (Vertex2 x y) (Vector2 dx dy)) = Events.MouseMotion x y dx dy
toSDLEvent (MouseButton Pressed (Vertex2 x y) b) = Events.MouseButtonDown x y b
toSDLEvent (MouseButton Released (Vertex2 x y) b) = Events.MouseButtonUp x y b
toSDLEvent (Resize w h) = Events.VideoResize w h
toSDLEvent Expose = Events.VideoExpose
toSDLEvent Quit = Events.Quit
toSDLEvent _ = Events.Unknown

fromSDLEvent :: Events.Event -> Maybe Event
fromSDLEvent (Events.GotFocus l) = Just $ Focus Gained l
fromSDLEvent (Events.LostFocus l) = Just $ Focus Lost l
fromSDLEvent (Events.KeyDown k) = Just $ Key Pressed k
fromSDLEvent (Events.KeyUp k) = Just $ Key Released k
fromSDLEvent (Events.MouseMotion x y dx dy) = Just $ MouseMotion (Vertex2 x y) (Vector2 dx dy)
fromSDLEvent (Events.MouseButtonDown x y b) = Just $ MouseButton Pressed (Vertex2 x y) b
fromSDLEvent (Events.MouseButtonUp x y b) = Just $ MouseButton Released (Vertex2 x y) b
fromSDLEvent (Events.VideoResize w h) = Just $ Resize w h
fromSDLEvent Events.VideoExpose = Just $ Expose
fromSDLEvent Events.Quit = Just $ Quit
fromSDLEvent Events.NoEvent = Nothing
fromSDLEvent _ = Just Unknown

tryPushEvent :: Event -> IO Bool
tryPushEvent = Events.tryPushEvent . toSDLEvent

pushEvent :: Event -> IO ()
pushEvent = Events.pushEvent . toSDLEvent

pollEvent :: IO (Maybe Event)
pollEvent = fromSDLEvent <$> Events.pollEvent

waitEvent :: IO Event
waitEvent = fromJust . fromSDLEvent <$> Events.waitEvent

waitEventBlocking :: IO Event
waitEventBlocking = fromJust . fromSDLEvent <$> Events.waitEventBlocking
