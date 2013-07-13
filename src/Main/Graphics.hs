{-# LANGUAGE NoImplicitPrelude
           #-}
module Main.Graphics ( initOpenGL
                     , updateGraphics
                     , resize
                     ) where

import Prelewd

import IO

import Control.Stream
import Data.Tuple
import Data.Tuple.All

import Game.Object
import Game.Physics
import Game.Render
import Game.State
import Game.Vector
import Util.Unit
import Wrappers.Events
import Wrappers.GLFW
import Wrappers.OpenGL hiding (position)

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 16

-- | Background color
bgColor :: Num a => (a, a, a, a)
bgColor = (0, 175, 200, 0)

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
focusPlayer g = let position = negate $ unitless <$> posn (phys $ fst $ object (player g) g)
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

-- | One iteration of graphics
updateGraphics :: Stream IO GameState ()
updateGraphics = lift $ arr $ \g -> drawFrame g >> io swapBuffers
