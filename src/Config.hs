-- | Settings are stored in this module
module Config ( viewDist
              , initState
              , jumpSpeed
              , moveSpeed
              , title
              , windowSize
              , bgColor
              ) where

import Data.Foldable (toList)

import Game.Object
import Game.Physics
import Game.State
import Game.Vector

toVec :: Num a => [a] -> Vector a
toVec = vector 0 . zip (toList dimensions)

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 16

-- | Start state of the game world
initState :: GameState
initState = foldr addObject emptyState [ Platform $ Physics (toVec [4, 1]) (toVec [-3, -1]) 0 0
                                       , Player $   Physics (toVec [1, 2]) (toVec [-3, 0])  0 gravity
                                       ]

-- | Speed boost for a jump
jumpSpeed :: Coord
jumpSpeed = 12

-- | Speed boost for movement
moveSpeed :: Coord
moveSpeed = 8

-- | Title of the game window
title :: String
title = "Game"

-- | Dimensions of the game window
windowSize :: Integral a => (a, a)
windowSize = (800, 600)

-- | Background color
bgColor :: Num a => (a, a, a, a)
bgColor = (0, 175, 200, 0)
