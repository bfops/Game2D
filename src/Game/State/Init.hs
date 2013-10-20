{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Functions for dealing with game state
module Game.State.Init ( initState
                       ) where

import Summit.Control.Stream
import Summit.Impure
import Summit.Num.Nonfinite
import Summit.Prelewd hiding (filter)
import Summit.Data.List (zip)

import Data.Tuple

import Game.Object
import Game.Physics
import Game.Vector
import Game.State
import qualified Game.Update.Physics as Physics
import Physics.Types
import Util.ID

-- | Acceleration due to gravity
gravity :: Vector Acceleration
gravity = singleV 0 Height (-32)

wraparound :: Bounds                -- ^ (lower, upper) dimensional bounds
           -> Position              -- ^ Position to wrap
           -> Position              -- ^ Position wrapped inside the bounds
wraparound = liftA2 $ \(start, end) s -> start + ((s - start) `mod` (end - start))

-- | Start state of the game world
initState :: GameState
initState = stateFromObjs [ Player   $ Physics (vec [1, 2])     1    (vec [-3,  0]) 0 gravity 0.1
                          , Platform $ Physics (vec [4, 1]) Infinite (vec [-3, -1]) 0    0    4
                          , Block    $ Physics (vec [1, 1])    0.1   (vec [-3,  3]) 0 gravity 0.8
                          , Platform $ Physics (vec [4, 1]) Infinite (vec [ 3, -1]) 0    0    2
                          , Platform $ Physics (vec [1, 4]) Infinite (vec [ 9,  1]) 0    0    8
                          , Platform $ Physics (vec [2, 1]) Infinite (vec [10,  1]) 0    0    8
                          , Platform $ Physics (vec [1, 4]) Infinite (vec [12,  1]) 0    0    8
                          ]
    where
        vec :: [a] -> Vector a
        vec = vector undefined . zip (toList dimensions)

        stateFromObjs = foldl' (\s o -> name (o, \i -> objectBehavior i o) s) mempty
                    >>> mapWithID (\i -> map ($ i))

objectBehavior :: ID -> GameObject -> ObjectBehavior
objectBehavior i = loop $ barr $ \ins -> phys' (vcty' $ \_-> setVcty ins)
                                     >>> Physics.update (dt ins) (phys <$> named (allObjects ins)) i
                                     >>> map (phys' (posn' $ wraparound $ worldBounds ins))
                                     >>> id &&& snd
