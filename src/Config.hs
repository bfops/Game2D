-- | Settings are stored in this module
module Config ( viewDist
              , initState
              , jumpSpeed
              , moveSpeed
              , title
              , windowSize
              , bgColor
              , keymap
              ) where

import Prelewd

import Storage.List
import Storage.Map

import Game.Input
import Game.Object
import Game.Physics
import Game.State
import Game.Vector
import Wrappers.Events

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 16

-- | Start state of the game world
initState :: GameState
initState = stateFromObjs [ Platform $ Physics (toPosn [4, 1]) (toPosn [-3, -1]) 0 0       1
                          , Player   $ Physics (toPosn [1, 2]) (toPosn [-3, 0])  0 gravity 1
                          ]
    where
        stateFromObjs = foldr addObject emptyState
        toPosn = fmap dist . vector 0 . zip (toList dimensions)

-- | Speed boost for a jump
jumpSpeed :: Speed
jumpSpeed = speed 12

-- | Speed boost for movement
moveSpeed :: Speed
moveSpeed = speed 8

-- | Acceleration due to gravity
gravity :: Vector Acceleration
gravity = accel <$> singleV 0 Height (-32)

-- | Title of the game window
title :: Text
title = "Game"

-- | Dimensions of the game window
windowSize :: Integral a => (a, a)
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
       ]
