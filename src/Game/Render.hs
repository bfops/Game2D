module Game.Render ( Drawable (..)
                   ) where

import Prelude ()
import Util.Prelewd

import Game.Logic

import Util.Impure

import Types

import Wrappers.OpenGL

-- | Convert the game's vector to an OpenGL coordinate
toGLVertex :: (Fractional a, Real a) => Vector a -> Vertex2 GLdouble
toGLVertex (Vector x y) = on Vertex2 realToFrac x y

-- | Things which can be drawn
class Drawable d where
    -- | Render the object to the screen
    draw :: d -> IO ()

instance Drawable GameState where
    draw (GameState objs) = mapM_ draw objs

-- | `draw c o` draws `o` as a quadrilateral, based on its position and size.
drawQuad :: Color4 GLdouble -> GameObject -> IO ()
drawQuad c o = renderPrimitive Quads $ drawColored c [ Vertex2 x  y
                                                     , Vertex2 x  y'
                                                     , Vertex2 x' y'
                                                     , Vertex2 x' y
                                                     ]
        where
            (Vertex2 x y) = toGLVertex (posn o)
            (Vertex2 x' y') = toGLVertex (posn o) <&> (+) <*> toGLVertex (size o)

instance Drawable GameObject where
    draw g
        | objType g == Block = drawQuad blue g
        | otherwise          = drawQuad red g
