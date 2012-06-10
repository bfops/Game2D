module Wrappers.OpenGL ( module OGL
                       , ColorDef (..)
                       , magenta
                       , forest
                       , toGLColor
                       , drawColored
                       ) where

import Graphics.Rendering.OpenGL as OGL

class ColorDef c where
    red, blue, lime, green, orange, yellow, cyan, pink, purple, white, black, transparent :: c

instance ColorDef (Color4 GLdouble) where
    red = Color4 1 0 0 0
    blue = Color4 0 0 1 0
    lime = Color4 0 1 0 0
    green = Color4 0 0.5 0 0
    orange = Color4 1 0.5 0 0
    yellow = Color4 1 1 0 0
    cyan = Color4 0 1 1 0
    pink = Color4 1 0 1 0
    purple = Color4 0.5 0 1 0
    white = Color4 1 1 1 0
    black = Color4 0 0 0 0
    transparent = Color4 0 0 0 1

magenta, forest :: ColorDef c => c
magenta = pink
forest = green

toGLColor :: Integral a => Color4 a -> Color4 GLclampf
toGLColor (Color4 r g b a) = Color4 (realToFrac r / 255) (realToFrac g / 255) (realToFrac b / 255) (realToFrac a / 255)

drawColored :: (Color c, Vertex v) => c -> [v] -> IO ()
drawColored c vs = color c >> mapM_ vertex vs
