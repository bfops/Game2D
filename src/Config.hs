-- | Settings are stored in this module
module Config ( viewDist
              , initState
              ) where

import Game.Physics
import Game.Types

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 16

-- | Start state of the game world
initState :: GameState
initState = foldr addObject emptyState [ Platform $ Physics (Vector 4 1) (Vector (-3) (-1)) (Vector 0 0) (Vector 0 0)
                                       , Player $ Physics (Vector 1 2) (Vector (-3) 0) (Vector 0 0) gravity
                                       ]
