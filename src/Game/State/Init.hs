{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Functions for dealing with game state
module Game.State.Init ( initState
                       ) where

import Prelewd hiding (filter)

import Impure

import Control.Stream
import Data.Tuple
import Num.Nonfinite
import Storage.List (zip)

import Game.Object
import Game.Physics
import Game.State
import Game.Vector
import qualified Game.Update.Physics as Physics
import Physics.Types

-- | Acceleration due to gravity
gravity :: Vector Acceleration
gravity = singleV 0 Height (-32)

-- | Edge for the game world
border :: Bounds
border = vector undefined
       $ [ (Width , (-12, 22))
         , (Height, ( -8, 12))
         ]

wraparound :: Bounds                -- ^ (lower, upper) dimensional bounds
           -> Position              -- ^ Position to wrap
           -> Position              -- ^ Position wrapped inside the bounds
wraparound = liftA2 $ \(start, end) s -> start + ((s - start) `mod` (end - start))

-- | Start state of the game world
initState :: GameState
initState = stateFromObjs [ Platform $ Physics (vec [4, 1]) Infinite (vec [-3, -1]) 0    0    4
                          , Platform $ Physics (vec [4, 1]) Infinite (vec [ 3, -1]) 0    0    2
                          , Platform $ Physics (vec [1, 4]) Infinite (vec [ 9,  1]) 0    0    8
                          , Platform $ Physics (vec [2, 1]) Infinite (vec [10,  1]) 0    0    8
                          , Platform $ Physics (vec [1, 4]) Infinite (vec [12,  1]) 0    0    8
                          , Player   $ Physics (vec [1, 2])     1    (vec [-3,  0]) 0 gravity 0.1
                          , Block    $ Physics (vec [1, 1])    0.1   (vec [-3,  3]) 0 gravity 0.8
                          ]
    where
        vec :: [a] -> Vector a
        vec = vector undefined . zip (toList dimensions)

        stateFromObjs = foldr (addObject =<< objectBehavior) $ emptyState border

objectBehavior :: GameObject -> ObjectBehavior
objectBehavior = loop $ barr $ \ins -> phys' (vcty' $ \_-> setVcty ins)
                                   >>> Physics.update (dt ins) (phys <$> allObjects ins) (objId ins)
                                   >>> map (phys' (posn' $ wraparound $ worldBounds ins))
                                   >>> id &&& snd
