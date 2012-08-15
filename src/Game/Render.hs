module Game.Render ( Drawable (..)
                   ) where

import Prelude ()
import Util.Prelewd

import Game.Physics
import Game.Types

import Util.IO

import Wrappers.OpenGL

-- | Convert the game's vector to an OpenGL coordinate
toGLVertex :: Real a => Vector a -> Vertex2 GLdouble
toGLVertex (Vector x y) = on Vertex2 realToFrac x y

-- | Things which can be drawn
class Drawable d where
    -- | Render the object to the screen
    draw :: d -> IO ()

instance Drawable GameState where
    draw = mapM_ (draw . val) . objects

-- | `draw c o` draws `o` as a quadrilateral, based on its position and size.
drawQuad :: Color4 GLdouble -> GameObject -> IO ()
drawQuad c o = renderPrimitive Quads $ drawColored c [ Vertex2 x  y
                                                     , Vertex2 x  y'
                                                     , Vertex2 x' y'
                                                     , Vertex2 x' y
                                                     ]
        where
            p = posn $ phys o
            (Vertex2 x y) = toGLVertex p
            (Vertex2 x' y') = toGLVertex p <&> (+) <*> toGLVertex (size $ phys o)

instance Drawable GameObject where
    draw g = drawQuad objColor g
        where
            objColor
                | isBlock g = blue
                | isPlayer g = green
                | otherwise = red
