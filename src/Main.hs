-- | Main module, entry point
module Main (main) where

import Util.Prelewd

import Control.Concurrent
import Control.Arrow hiding (loop)
import Data.Tuple.Curry
import qualified System.IO as IO

import Game.Input
import Game.Physics hiding (Size)
import Game.Render
import Game.State
import Game.Update
import Util.Impure
import Util.IO
import Util.Map

import Wrappers.Events
import Wrappers.OpenGL as OGL hiding (windowPos)

import qualified Graphics.UI.GLFW as GLFW

import Config

-- | Program state
data State = State { game       :: GameState
                   , lastUpdate :: Double
                   }

game' :: (GameState -> GameState) -> State -> State
game' f s = s { game = f (game s) }

initOpenGL :: IO ()
initOpenGL = io $ do
            shadeModel $= Smooth
            clearDepth $= 1
            depthFunc $= Just Less
            hint PerspectiveCorrection $= Nicest

-- | Run the action within a GLFW-initialized state, and close it afterward
runGLFW :: IO a -> IO a
runGLFW body =  initGLFW
             >> body
             <* closeGLFW

initGLFW :: IO ()
initGLFW = io $ do
        True <- GLFW.initialize
        True <- GLFW.openWindow (uncurryN Size windowSize) [] GLFW.Window

        GLFW.windowPos $= Position 0 0
        GLFW.windowTitle $= title

closeGLFW :: IO ()
closeGLFW = io $  GLFW.closeWindow
               >> GLFW.terminate

drawFrame :: GameState -- ^ State to draw
          -> IO ()
drawFrame g = do
        -- Clear the screen
        io $ do clear [ ColorBuffer, DepthBuffer ]
                -- Reset the view
                loadIdentity

                -- Move to the render location
                translate $ Vector3 0 0 (negate $ fromIntegral viewDist :: GLdouble)

        draw g
        -- Write it all to the buffer
        io flush

-- | Resize OpenGL view
resize :: Size -> IO ()
resize s@(Size w h) = io $ do
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
clearResizeEvents :: EventPoller -> IO ()
clearResizeEvents poll = tryResize . last =<< poll [ResizeEvents]
    where
        tryResize = maybe (return ()) resize'

        resize' :: Event -> IO ()
        resize' (ResizeEvent s) = resize s
        resize' _ = error "poll [ResizeEvents] returned an invalid list."

clearRefreshEvents :: EventPoller -> IO ()
clearRefreshEvents poll = poll [RefreshEvents] $> ()

-- | Receive all pending input events, and convert them to game input
getInputs :: EventPoller -> IO [(Input, ButtonState)]
getInputs poll = mapMaybe rawToInput <$> poll [ ButtonEvents Nothing Nothing, MouseMoveEvents ]
    where
        -- Convert an input event to a game input
        rawToInput :: Event -> Maybe (Input, ButtonState)
        rawToInput (ButtonEvent (KeyButton key) s) = lookup key keymap <&> (, s)
        rawToInput _ = Nothing

        keys :: Map Key Input
        keys = fromList $ first CharKey <$>
                [ (' ', Jump)
                , ('W', Jump)
                , ('A', Left)
                , ('D', Right)
                ]

-- | Update the program state with input and time elapsed
newState :: State -> [(Input, ButtonState)] -> Double -> State
newState s is t = game' (update is deltaT) $ s { lastUpdate = t }
    where
        deltaT = time $ realToFrac $ t - lastUpdate s

mainLoop :: EventPoller -> State -> IO State
mainLoop poll s0 =   isOpen poll
                 >>= guard
                 >>  visualize
                 >>  updateState
    where
        updateState = newState s0 <$> getInputs poll <*> io (get GLFW.time)

        visualize = do
            -- Since we're drawing, all the window refresh events are taken care of
            clearRefreshEvents poll
            clearResizeEvents poll
            drawFrame $ game s0
            -- Double buffering
            io GLFW.swapBuffers

-- | Create initial program state
getInitState :: IO State
getInitState = State initState <$> io (get GLFW.time)

-- | Run the program
main :: IO.IO ()
main = runIO $ runGLFW $ do
        initOpenGL
        let glColor = uncurryN Color4 bgColor
        clearColor $= toGLColor (glColor :: Color4 GLubyte)

        poll <- createEventPoller
        run $ loop (mainLoop poll) =<< getInitState

loop :: Monad m => (a -> m a) -> a -> m a
loop f x = f x >>= loop f
