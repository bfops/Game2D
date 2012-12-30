module Main.Graphics ( initOpenGL
                     , updateGraphics
                     ) where

import Prelewd

import IO

import Impure

import Data.Tuple.All
import Storage.List

import Config

import Game.Object
import Game.Physics (posn)
import Game.Render
import Game.State
import Game.Vector
import Util.Unit
import Wrappers.Events
import Wrappers.GLFW
import Wrappers.OpenGL hiding (position)

-- | Initialize the OpenGL context
initOpenGL :: IO ()
initOpenGL = io $ do
        shadeModel $= Smooth
        clearDepth $= 1
        depthFunc $= Just Less
        hint PerspectiveCorrection $= Nicest

        let glColor = uncurryN Color4 bgColor
        clearColor $= toGLColor (glColor :: Color4 GLubyte)

-- | Draw one frame of the game state
drawFrame :: GameState -- ^ State to draw
          -> IO ()
drawFrame g = do
        -- Clear the screen
        io $ clear [ ColorBuffer, DepthBuffer ]
        -- Reset the view
        io loadIdentity
        
        focusPlayer g
        draw g

        -- Write it all to the buffer
        io flush

-- | Shift the render location so that the player is focused
focusPlayer :: GameState -> IO ()
focusPlayer g = let position = negate $ map unitless $ posn $ phys $ object (player g) g
                    Vector x y = realToFrac . (`component` position) <$> dimensions
                in io $ translate $ Vector3 x y (negate $ fromIntegral viewDist :: GLdouble)

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

-- | Take care of all received resize events
clearResizeEvents :: EventPoller -> IO ()
clearResizeEvents poll = tryResize . last =<< poll [ResizeEvents]
    where
        tryResize = maybe (return ()) resizeEvent

        resizeEvent :: Event -> IO ()
        resizeEvent (ResizeEvent s) = resize s
        resizeEvent _ = error "poll [ResizeEvents] returned an invalid list."

-- | Remove all refresh events from the event poller
clearRefreshEvents :: EventPoller -> IO ()
clearRefreshEvents poll = poll [RefreshEvents] $> ()

-- | One iteration of graphics
updateGraphics :: EventPoller -> GameState -> IO ()
updateGraphics poll g = do
    -- Since we're drawing, all the window refresh events are taken care of
    clearRefreshEvents poll
    clearResizeEvents poll

    drawFrame g

    -- Double buffering
    io swapBuffers
