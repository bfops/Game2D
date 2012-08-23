-- | Main module, entry point
module Main (main) where

import Prelude ()
import Util.Prelewd

import Control.Arrow
import Control.Concurrent
import Data.Tuple.Curry

import Game.Input
import Game.Physics hiding (Size)
import Game.Render
import Game.State
import Game.Update
import Util.Impure
import Util.IO
import Util.Unit

import Wrappers.Events
import Wrappers.OpenGL as OGL hiding (windowPos)
import qualified Graphics.UI.GLFW as GLFW

import Config

-- | Program state
data State = State { game       :: GameState
                   , lastUpdate :: Double
                   }

initOpenGL :: IO ()
initOpenGL = do
            shadeModel $= Smooth
            clearDepth $= 1
            depthFunc $= Just Less
            hint PerspectiveCorrection $= Nicest

drawFrame :: GameState -- ^ State to draw
          -> IO ()
drawFrame g = do
        -- Clear the screen
        clear [ ColorBuffer, DepthBuffer ]
        -- Reset the view
        loadIdentity

        -- Move to the render location
        translate $ Vector3 0 0 (negate $ fromIntegral viewDist :: GLdouble)

        draw g

        -- Write it all to the buffer
        flush

-- | Resize OpenGL view
resize :: Size -> IO ()
resize s@(Size w h) = do
    viewport $= (Position 0 0, s)

    matrixMode $= Projection
    loadIdentity
    perspective 45 (on (/) realToFrac w h) 0.1 64

    matrixMode $= Modelview 0
    loadIdentity

-- | Is the window open?
isOpen :: EventPoller -> IO Bool
isOpen poll = null <$> poll [CloseEvents]

-- | Take care of all received resize events
handleResize :: EventPoller -> IO ()
handleResize poll = poll [ResizeEvents] >>= maybe (return ()) resize' . last
    where
        resize' :: Event -> IO ()
        resize' (ResizeEvent s) = resize s
        resize' _ = error "poll [ResizeEvents] returned an invalid list."

-- | Receive all pending input events, and convert them to game input
getInputs :: EventPoller -> IO [Input]
getInputs poll = mapMaybe rawToInput <$> poll [ ButtonEvents Nothing Nothing, MouseMoveEvents ]
    where
        -- Convert an input event to a game input
        rawToInput :: Event -> Maybe Input
        rawToInput (ButtonEvent (KeyButton key) Press) = lookup key keys
        rawToInput _ = Nothing

        keys :: [(Key, Input)]
        keys = fmap (first CharKey)
                [ (' ', Jump)
                , ('W', Jump)
                , ('A', Left)
                , ('D', Right)
                ]

-- | Update the program state with input and time elapsed
newState :: State -> [Input] -> Double -> State
newState s is t = let deltaT = (realToFrac $ t - lastUpdate s) `unit` Time
                  in s { lastUpdate = t
                       , game = update is deltaT $ game s
                       }

mainLoop :: EventPoller -> State -> IO ()
mainLoop poll s0 = isOpen poll >>= bool (return ()) runLoop
    where
        runLoop =   visualize
                >>  updateState
                <*  threadDelay 10000
                >>= mainLoop poll

        updateState = newState s0 <$> getInputs poll <*> get GLFW.time

        visualize = do
            -- Since we're drawing, all the window refresh events are taken care of
            _ <- poll [RefreshEvents]
            handleResize poll
            drawFrame $ game s0
            -- Double buffering
            GLFW.swapBuffers

-- | Create initial program state
getInitState :: IO State
getInitState = State initState <$> get GLFW.time

-- | Run the program
main :: IO ()
main = do
        True <- GLFW.initialize
        True <- GLFW.openWindow (uncurryN Size windowSize) [] GLFW.Window

        GLFW.windowPos $= Position 0 0
        GLFW.windowTitle $= title

        initOpenGL
        let glColor = uncurryN Color4 bgColor
        clearColor $= toGLColor (glColor :: Color4 GLubyte)

        poll <- createEventPoller

        getInitState >>= mainLoop poll

        GLFW.closeWindow
        GLFW.terminate
