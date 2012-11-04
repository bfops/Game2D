-- | Main module, entry point
module Main (main) where

import Prelewd

import Impure
import IO

import Data.Tuple
import Data.Tuple.Curry
import Storage.List
import Storage.Map

import Game.Input
import Game.Object
import Game.Physics hiding (Size)
import Game.Render
import Game.State
import Game.Update
import Game.Vector

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

        let glColor = uncurryN Color4 bgColor
        clearColor $= toGLColor (glColor :: Color4 GLubyte)

-- | Run the action within a GLFW-initialized state, and close it afterward
runGLFW :: IO a -> IO a
runGLFW body =  initGLFW
             >> body
             <* closeGLFW

initGLFW :: IO ()
initGLFW = io $ do
        True <- GLFW.initialize
        True <- GLFW.openWindow $ GLFW.defaultDisplayOptions
                    { GLFW.displayOptions_width = fst windowSize
                    , GLFW.displayOptions_height = snd windowSize
                    , GLFW.displayOptions_windowIsResizable = False
                    }

        GLFW.setWindowPosition 0 0
        GLFW.setWindowTitle title

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

                let v = negate $ map fromDist $ posn $ phys $ object (player g) g
                    [x, y] = map (realToFrac.(`component` v)) [Width, Height]

                -- Move to the render location
                translate $ Vector3 x y (negate $ fromIntegral viewDist :: GLdouble)

        draw g
        -- Write it all to the buffer
        io flush

-- | Resize OpenGL view
resize :: Size -> IO ()
resize s@(Size w h) = io $ do
        viewport $= (Position 0 0, s)
    
        matrixMode $= Projection
        loadIdentity
        perspective 45 (w // h) 0.1 64
    
        matrixMode $= Modelview 0
        loadIdentity
    where
        (//) = (/) `on` realToFrac

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

-- | Update the program state with input and time elapsed
newState :: State -> [(Input, ButtonState)] -> Double -> State
newState s is t = let deltaT = time $ realToFrac $ t - lastUpdate s
                  in game' (update is deltaT) $ s { lastUpdate = t }

mainLoop :: EventPoller -> State -> IO State
mainLoop poll s0 =   isOpen poll
                 >>= guard
                 >>  visualize
                 >>  updateState
    where
        updateState = newState s0 <$> getInputs poll <*> io GLFW.getTime

        visualize = do
            -- Since we're drawing, all the window refresh events are taken care of
            clearRefreshEvents poll
            clearResizeEvents poll
            drawFrame $ game s0
            -- Double buffering
            io GLFW.swapBuffers

-- | Create initial program state
getInitState :: IO State
getInitState = State initState <$> io GLFW.getTime

-- | Run the program
main :: SystemIO ()
main = runIO $ runGLFW $ do
        initOpenGL
        poll <- createEventPoller
        quiet $ loop (mainLoop poll) =<< getInitState

loop :: Monad m => (a -> m a) -> a -> m a
loop f x = f x >>= loop f
