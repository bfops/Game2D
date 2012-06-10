module Main (main) where

import Prelude ()
import Util.Prelewd

import Control.Concurrent

import Util.Impure

import Wrappers.Events
import Wrappers.OpenGL as OGL hiding (windowPos)
import Graphics.UI.GLFW as GLFW

import Game.Logic
import Game.Render

import Config

-- | Program state
data State = State { game       :: GameState
                   , lastUpdate :: Double
                   }

initGL :: IO ()
initGL = do
            shadeModel $= Smooth
            clearDepth $= 1
            depthFunc $= Just Less
            hint PerspectiveCorrection $= Nicest

drawFrame :: GameState -- ^ State to draw
          -> IO ()
drawFrame g = do
                clear [ ColorBuffer, DepthBuffer ]
                loadIdentity

                translate $ Vector3 0 0 (negate $ fromIntegral viewDist :: GLdouble)
                draw g

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

    flush

-- | Is the window open?
isOpen :: EventPoller -> IO Bool
isOpen poll = null <$> poll [CloseEvents]

handleResize :: EventPoller -> IO ()
handleResize poll = poll [ResizeEvents] >>= maybe (return ()) resize' . last
    where
        resize' :: Event -> IO ()
        resize' (ResizeEvent s) = resize s
        resize' _ = error "poll [ResizeEvents] returned an invalid list."

getInputs :: EventPoller -> IO [Input]
getInputs poll = mapMaybe rawToInput <$> poll [ ButtonEvents Nothing Nothing, MouseMoveEvents ]
    where
        rawToInput :: Event -> Maybe Input
        rawToInput (ButtonEvent (KeyButton (SpecialKey UP)) Press) = Just MoveUp
        rawToInput _ = Nothing

updateState :: State -> [Input] -> Double -> State
updateState s is t = s { lastUpdate = t
                       , game = update (game s) is $ t - (lastUpdate s)
                       }

mainLoop :: EventPoller -> State -> IO ()
mainLoop poll s0 = isOpen poll >>= bool (return ()) (loopBody >>= mainLoop poll)
    where
        loopBody :: IO State
        loopBody = do
            threadDelay 10000
            visualize
            fullUpdate

        fullUpdate = updateState s0 <$> getInputs poll <*> get time

        visualize = do 
            _ <- poll [RefreshEvents]
            drawFrame $ game s0
            swapBuffers
            handleResize poll

main :: IO ()
main = do
        let c = Color4 0 175 200 0 :: Color4 GLubyte
        
        True <- initialize
        True <- openWindow (Size 800 600) [] Window 

        GLFW.windowPos $= Position 0 0
        windowTitle $= "Eria"
        
        initGL
        -- Set background color
        clearColor $= toGLColor c
        
        poll <- createEventPoller
        
        mainLoop poll . State initState =<< get time
        closeWindow
        terminate
