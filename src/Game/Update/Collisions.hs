{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Update game state's physical interactions
module Game.Update.Collisions ( update
                              , Collisions
                              ) where

import Prelewd

import Impure

import Storage.Map
import Storage.Member
import Storage.Pair
import Storage.Set

import Subset.Num

import Game.Physics
import Game.Object
import Game.State
import Game.Vector
import Physics.Friction

-- | Transform Physics properties of two GameObjects
phys2' :: (Pair Physics -> Pair Physics) -> Pair GameObject -> Pair GameObject
phys2' f gs =  phys' . (\x _->x) <$> f (phys <$> gs) <*> gs

-- | Apply a function to a pair of objects in a GameState
object2' :: (Pair GameObject -> Pair GameObject) -> Pair ID -> GameState -> GameState
object2' f ids g = foldr ($) g $ object' . (\x _->x) <$> f (ids <&> (`object` g)) <*> ids

keepDims :: Set Dimension -> Vector a -> Vector (Maybe a)
keepDims dims = liftA2 (mcond . (`elem` dims)) dimensions

subDims :: Set Dimension -> Vector a -> Vector a -> Vector a
subDims = liftA2 (<?>) <$$> keepDims

-- | The Velocity transferred in a collision
type Collisions = Map (Pair ID) (Set Dimension)

-- | Run both objects' collision functions
collideBoth :: Pair ID -> Set Dimension -> GameState -> GameState
collideBoth ids dims = object2' (phys2' $ collide <*> inelastic <*> equilibrium) ids
    where
        collide ps eqs t = let toTransfer = toNat . abs <$> t <&> (<?> error "abs returned negative value")
                           in friction (keepDims dims $ toTransfer)
                            $ pair (vcty' . subDims dims . vcty) <$> sequence (Pair eqs ps)

-- | Advance a game state based on collisions
update :: Collisions -> GameState -> GameState
update cs g = foldrWithKey collideBoth g cs
