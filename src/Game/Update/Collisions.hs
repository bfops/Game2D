-- | Update game state's physical interactions
module Game.Update.Collisions ( update
                              , Collisions
                              ) where

import Prelewd

import Num.Nonfinite
import Num.Positive
import Storage.Map hiding (difference)
import Storage.Member
import Storage.Pair
import Storage.Set

import Game.Physics
import Game.Object
import Game.State
import Game.Vector
import Util.Unit

-- | Map object interactions to their collision dimensions
type Collisions = Map (Pair ID) (Set Dimension)

toSet :: (Foldable t, Ord a) => t a -> Set a
toSet = set . toList

toDouble :: Real a => a -> Double
toDouble = realToFrac

fromDouble :: Fractional b => Double -> b
fromDouble d = let precision = 100 :: Integer
               in ((/) `on` fromIntegral) (round $ fromIntegral precision * d) precision

setSeveral :: Foldable t => a -> t Dimension -> Vector a -> Vector a
setSeveral x = flip $ foldr (`setV` x)

divPosInf :: (Ord a, Fractional a) => Positive a -> Nonfinite (Positive a) -> a
divPosInf x y = mapInfinite ((num x /) . num <$> y) 0

divInfUnit :: (Ord a, Fractional a, UnitMult x y z) => Unit z a -> Nonfinite (Positive (Unit y a)) -> Unit x a
divInfUnit z x = mapInfinite ((z /&) . num <$> x) 0

-- | Apply a function to a pair of objects in a GameState
object2' :: (Pair GameObject -> Pair GameObject) -> Pair ID -> GameState -> GameState
object2' f ids g = foldr ($) g $ object' . const <$> f (ids <&> (`object` g)) <*> ids

phys2' :: (Pair Physics -> Pair Physics) -> Pair GameObject -> Pair GameObject
phys2' f gs =  phys' . const <$> f (phys <$> gs) <*> gs

vcty2' :: (Pair Velocity -> Pair Velocity) -> Pair Physics -> Pair Physics
vcty2' f ps = vcty' . const <$> f (vcty <$> ps) <*> ps

-- | Run both objects' collision functions
collideBoth :: Time -> Pair ID -> Set Dimension -> GameState -> GameState
collideBoth t ids dims = object2' (applyFriction t dims . collideInelast) ids
    where
        collideInelast :: Pair GameObject -> Pair GameObject
        collideInelast = map =<< phys' . vcty' . collideSet . inelastic

        -- | Set velocity in the collision dimensions
        collideSet :: Velocity -> Velocity -> Velocity
        collideSet = liftA3 (iff . (`elem` dims)) dimensions

inelastic :: Pair GameObject -> Velocity
inelastic objs = let Pair m1 m2 = map num . mass . phys <$> objs
                     Pair v1 v2 = vcty . phys <$> objs
                 in (term m2 m1 <$> v1) + (term m1 m2 <$> v2)
    where
        term m2 m1 v1 = let (Finite x) = Finite v1 / (Unit . unitless <$> 1 + m2/m1)
                        in x

-- | Advance a game state based on collisions
update :: Time -> Collisions -> GameState -> GameState
update t cs g = foldrWithKey (collideBoth t) g cs

applyFriction :: Time -> Set Dimension -> Pair GameObject -> Pair GameObject
applyFriction t dims objs = let
            fric = friction t dims $ phys <$> objs
            equal = equilibrium $ phys <$> objs
            toTransfer = transfer1 <$> fric <*> equal
        in phys2' (vcty2' $ transfer (mass . phys <$> objs) toTransfer) objs
    where
        transfer1 :: Nonfinite Momentum -> Momentum -> Momentum
        transfer1 x y = signum (mapInfinite x y) * mapInfinite (min (abs y) . abs <$> x) y

-- | Momentum to transfer due to friction
friction :: Time -> Set Dimension -> Pair Physics -> Vector (Nonfinite Momentum)
friction t dims (Pair obj collidee) = let
            norm = setSeveral 0 (toSet dimensions `difference` dims) $ accl obj
            normMagnitude = Unit $ fromDouble $ magnitude $ toDouble <$> norm
            transferSpeed = mu obj * mu collidee &* (num t &* normMagnitude)
            maxTransfer :: Nonfinite Momentum
            maxTransfer = (transferSpeed &*) . num <$> mass obj
            direction = normalize $ setSeveral 0 dims $ toDouble <$> ((-) `on` vcty) obj collidee
        in liftA2 (*&) maxTransfer . Finite . fromDouble <$> direction

-- | Determine the momentum transfer required to reach velocity equilibrium
equilibrium :: Pair Physics -> Vector Momentum
equilibrium ps = let Pair v1 v2 = vcty <$> ps
                 in equilibrium1 (mass <$> ps) <$> v1 <*> v2
    where
        equilibrium1 :: Pair Mass -> Speed -> Speed -> Momentum
        -- if momentum transferred is t, at equilibrium:
        --    => v1 - t/m1 = v2 + t/m2
        --    => m1*m2*v1 - m2*t = m1*m2*v2 + m1*t
        --    => m1 * m2 * (v1 - v2) = t * (m1 + m2)
        --    => t = (v1 - v2) * m1 * m2 / (m1 + m2)
        --    =>   = (v1 - v2) * 1 / (1/m2 + 1/m1)
        equilibrium1 (Pair m1 m2) v1 v2 = (v1 - v2) &* (1 / ((+) `on` divPosInf 1) m1 m2)

transfer :: Pair Mass -> Vector Momentum -> Pair Velocity -> Pair Velocity
transfer ms = sequence .$ liftA2 transfer1 .^ sequence
    where
        transfer1 :: Momentum -> Pair Speed -> Pair Speed
        transfer1 t vs = vs <&> (+) <*> (Pair (negate t) t <&> divInfUnit <*> ms)
