-- | Render the game state
module Game.Render ( Drawable (..)
                   ) where

import Prelewd

import IO

import Num.Positive

import Game.Object
import Game.Physics
import Game.State
import Game.Vector

import Wrappers.OpenGL hiding (Size, Position)

-- | Convert the game's vector to an OpenGL coordinate
toGLVertex :: Position -> Vertex2 GLdouble
toGLVertex v = on Vertex2 realToFrac (component Width v) (component Height v)

-- | Things which can be drawn
class Drawable d where
    -- | Render the object to the screen
    draw :: d -> IO ()

instance Drawable GameState where
    draw = mapM_ draw . objects

-- | `draw c o` draws `o` as a quadrilateral, based on its position and size.
drawQuad :: Color4 GLdouble -> GameObject -> IO ()
drawQuad c o = io $ renderPrimitive Quads $ runIO $ drawColored c
                [ Vertex2 x  y
                , Vertex2 x  y'
                , Vertex2 x' y'
                , Vertex2 x' y
                ]
        where
            p = posn $ phys o
            (Vertex2 x y) = toGLVertex p
            (Vertex2 x' y') = toGLVertex p <&> (+) <*> toGLVertex (map num $ size $ phys o)

instance Drawable GameObject where
    draw g = drawQuad objColor g
        where
            objColor
                | isBlock g = blue
                | isPlayer g = green
                | otherwise = red
