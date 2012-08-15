-- | Settings are stored in this module
module Config ( viewDist
              , initState
              , jumpSpeed
              , moveSpeed
              , windowTitle
              , windowDimensions
              , bgColor
              ) where

import Game.Object
import Game.Physics
import Game.State

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 16

-- | Start state of the game world
initState :: GameState
initState = foldr addObject emptyState [ Platform $ Physics (Vector 4 1) (Vector (-3) (-1)) (Vector 0 0) (Vector 0 0)
                                       , Player $ Physics (Vector 1 2) (Vector (-3) 0) (Vector 0 0) gravity
                                       ]

jumpSpeed :: Coord
jumpSpeed = 12

moveSpeed :: Coord
moveSpeed = 8

windowTitle :: String
windowTitle = "Game"

windowDimensions :: Integral a => (a, a)
windowDimensions = (800, 600)

bgColor :: Num a => (a, a, a, a)
bgColor = (0, 175, 200, 0)
