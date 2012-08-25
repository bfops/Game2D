-- | OpenGL with some extensions for color
module Wrappers.OpenGL ( module OGL
                       , ColorDef (..)
                       , magenta
                       , forest
                       , drawColored
                       , GLColor (..)
                       ) where

import Prelude ()
import Util.Prelewd

import Data.Tuple.Curry

import Util.IO

import Graphics.Rendering.OpenGL as OGL

-- | Class of types which can be used to define colors
class ColorDef c where
    red, blue, lime, green, orange, yellow, cyan, pink, purple, white, black, transparent :: c

instance ColorDef (Color4 GLdouble) where
    red         = Color4 1   0   0 0
    blue        = Color4 0   0   1 0
    lime        = Color4 0   1   0 0
    green       = Color4 0   0.5 0 0
    orange      = Color4 1   0.5 0 0
    yellow      = Color4 1   1   0 0
    cyan        = Color4 0   1   1 0
    pink        = Color4 1   0   1 0
    purple      = Color4 0.5 0   1 0
    white       = Color4 1   1   1 0
    black       = Color4 0   0   0 0
    transparent = Color4 0   0   0 1

instance ColorDef (GLubyte, GLubyte, GLubyte, GLubyte) where
    red         = (255, 0  , 0  , 0  )
    blue        = (0  , 0  , 255, 0  )
    lime        = (0  , 255, 0  , 0  )
    green       = (0  , 127, 0  , 0  )
    orange      = (255, 127, 0  , 0  )
    yellow      = (255, 255, 0  , 0  )
    cyan        = (0  , 255, 255, 0  )
    pink        = (255, 0  , 255, 0  )
    purple      = (127, 0  , 255, 0  )
    white       = (255, 255, 255, 0  )
    black       = (0  , 0  , 0  , 0  )
    transparent = (0  , 0  , 0  , 255)

-- | Color renames
magenta, forest :: ColorDef c => c
magenta = pink
forest = green

-- | Set the OpenGL color, then call `vertex` on each vertex
drawColored :: (Color c, Vertex v) => c -> [v] -> IO ()
drawColored c vs = color c >> mapM_ vertex vs
 
-- | Colors which can be used to color OpenGL's rendering
class GLColor c where
    -- | Convert to an OpenGL color
    toGLColor :: c -> Color4 GLclampf

instance GLColor (Color4 GLubyte) where
    toGLColor c = realToFrac <$> c <&> (/ 255)

instance GLColor (Color4 GLdouble) where
    toGLColor c = realToFrac <$> c

instance GLColor (GLubyte, GLubyte, GLubyte, GLubyte) where
    toGLColor = toGLColor . uncurryN Color4
