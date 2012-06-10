module Game.Render ( Drawable (..)
                   ) where

import Prelude ()
import Util.Prelewd

import Game.Logic

import Util.Impure
import Util.Types

import Wrappers.OpenGL

toGLVertex :: (Fractional a, Real a) => Vector a -> Vertex2 GLdouble
toGLVertex (Vector x y) = on Vertex2 realToFrac x y

class Drawable d where
    draw :: d -> IO ()

instance Drawable GameState where
    draw (GameState objs) = mapM_ draw objs

drawQuad :: Color4 GLdouble -> GameObject -> IO ()
drawQuad c p = renderPrimitive Quads $ drawColored c [ Vertex2 x  y
                                                     , Vertex2 x  y'
                                                     , Vertex2 x' y'
                                                     , Vertex2 x' y
                                                     ]
        where
            (Vertex2 x y) = toGLVertex (posn p)
            (Vertex2 x' y') = (+) <$> toGLVertex (posn p) <*> toGLVertex (size p)

instance Drawable GameObject where
    draw g
        | objType g == Block = drawQuad blue g
        | otherwise          = drawQuad red g
