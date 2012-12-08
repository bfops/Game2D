-- | Settings are stored in this module
module Config ( viewDist
              , initState
              , jumpSpeed
              , moveSpeed
              , speedCap
              , title
              , windowSize
              , bgColor
              , keymap
              ) where

import Prelewd

import Impure

import Num.Nonfinite
import Num.Positive
import Storage.List
import Storage.Map

import Game.Input
import Game.Object
import Game.Physics
import Game.State
import Game.Vector
import Util.Unit
import Wrappers.Events

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 16

vec :: [a] -> Vector a
vec = vector undefined . zip (toList dimensions)

-- | Start state of the game world
initState :: GameState
initState = stateFromObjs [ Platform $ Physics (vec [4, 1]) posInfinity (vec [-3, -1]) 0    0    2
                          , Platform $ Physics (vec [4, 1]) posInfinity (vec [ 3, -1]) 0    0    0.4
                          , Player   $ Physics (vec [1, 2])      1      (vec [-3,  0]) 0 gravity 1
                          ]
    where
        posInfinity = positive (Unit Infinite) <?> error "Infinity is negative"
        stateFromObjs = foldr addObject $ emptyState border

-- | Edge for the game world
border :: Bounds
border = vector undefined
       $ [ (Width , (-12, 16))
         , (Height, (-8, 8))
         ]

-- | Speed boost for a jump
jumpSpeed :: Positive Speed
jumpSpeed = 12

-- | Speed boost for movement
moveSpeed :: Positive Speed
moveSpeed = 0.3

-- | Fastest you can walk
speedCap :: Positive Speed
speedCap = 8

-- | Acceleration due to gravity
gravity :: Vector Acceleration
gravity = singleV 0 Height (-32)

-- | Title of the game window
title :: Text
title = "Game"

-- | Dimensions of the game window
windowSize :: Integral a => (Positive a, Positive a)
windowSize = (800, 600)

-- | Background color
bgColor :: Num a => (a, a, a, a)
bgColor = (0, 175, 200, 0)

-- | What controls what?
keymap :: Map Key Input
keymap = mapKeys CharKey $ fromList
       [ (' ', Jump)
       , ('W', Jump)
       , ('A', Left)
       , ('D', Right)
       , ('R', Reset)
       ]
