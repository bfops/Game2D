-- | Update game state's physical interactions
module Game.Update.Collisions ( update
                              , Collisions
                              ) where

import Prelewd

import Num.Nonfinite
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
import Physics.Types
import Util.Unit

-- | Map object interactions to their collision dimensions
type Collisions = Map (Pair ID) (Set Dimension)

-- | Apply a function to a pair of objects in a GameState
object2' :: (Pair GameObject -> Pair GameObject) -> Pair ID -> GameState -> GameState
object2' f ids g = foldr ($) g $ object' . (\x _->x) <$> f (ids <&> (`object` g)) <*> ids

-- | Run both objects' collision functions
collideBoth :: Time -> Pair ID -> Set Dimension -> GameState -> GameState
collideBoth t ids dims = object2' (applyFriction t dims . collideInelast) ids
    where
        collideInelast :: Pair GameObject -> Pair GameObject
        collideInelast = map =<< phys' . vcty' . collideSet . inelastic

        -- | Set velocity in the collision dimensions
        collideSet :: Velocity -> Velocity -> Velocity
        collideSet = liftA3 (iff . (`elem` dims)) dimensions

-- | The resultant velocity of an inelastic collision
inelastic :: Pair GameObject -> Velocity
inelastic objs = let Pair m1 m2 = map fromPos . mass . phys <$> objs
                     Pair v1 v2 = vcty . phys <$> objs
                 in (term m2 m1 <$> v1) + (term m1 m2 <$> v2)
    where
        term m2 m1 v1 = (\(Finite x)->x) $ Finite v1 / (Unit . unitless <$> 1 + m2/m1)

-- | Advance a game state based on collisions
update :: Time -> Collisions -> GameState -> GameState
update t cs g = foldrWithKey (collideBoth t) g cs
