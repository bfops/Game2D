{-# LANGUAGE NoImplicitPrelude
           #-}
module Physics.Friction ( applyFriction
                        ) where

import Prelewd

import Num.Nonfinite
import Storage.Pair
import Storage.Set
import Subset.Num

import Game.Physics
import Game.Vector
import Physics.Types
import Util.Unit

toSet :: (Foldable t, Ord a) => t a -> Set a
toSet = set . toList

setSeveral :: Foldable t => a -> t Dimension -> Vector a -> Vector a
setSeveral x = flip $ foldr (`setV` x)

divPosInf :: (Ord a, Fractional a) => Positive a -> Nonfinite (Positive a) -> a
divPosInf x y = mapInfinite ((fromPos x /) . fromPos <$> y) 0

divIndfUnit :: (Ord a, Fractional a, UnitMult x y z) => Unit z a -> Nonfinite (Positive (Unit y a)) -> Unit x a
divIndfUnit z x = mapInfinite ((z /&) . fromPos <$> x) 0

-- | Transform Velocities of two Physics objects
vcty2' :: (Pair Velocity -> Pair Velocity) -> Pair Physics -> Pair Physics
vcty2' f ps = vcty' . (\x _->x) <$> f (vcty <$> ps) <*> ps

fromDouble :: Double -> Unit t PhysicsValue
fromDouble d = let precision = 100
               in Unit $ ((/) `on` fromInteger) (round $ fromInteger precision * d) precision

applyFriction :: Time -> Set Dimension -> Pair Physics -> Pair Physics
applyFriction t dims objs = let
            fric = friction t dims objs
            equal = equilibrium objs
            toTransfer = transfer1 <$> fric <*> equal
        in vcty2' (transfer (mass <$> objs) toTransfer) objs
    where
        transfer1 :: Nonfinite Momentum -> Momentum -> Momentum
        transfer1 x y = signum (mapInfinite x y) * mapInfinite (min (abs y) . abs <$> x) y

-- | Momentum to transfer due to friction
friction :: Time -> Set Dimension -> Pair Physics -> Vector (Nonfinite Momentum)
friction t dims (Pair obj collidee) = let
            norm = setSeveral 0 (toSet dimensions `difference` dims) $ accl obj
            normMagnitude = fromDouble $ magnitude $ realToFrac <$> norm
            transferSpeed = mu obj * mu collidee &* (fromNat t &* normMagnitude)
            maxTransfer :: Nonfinite Momentum
            maxTransfer = (transferSpeed &*) . fromPos <$> mass obj
            direction = normalize $ setSeveral 0 dims $ realToFrac <$> ((-) `on` vcty) obj collidee
        in liftA2 (*&) maxTransfer . Finite . fromDouble <$> direction

-- | Determine the momentum transfer required to reach velocity equilibrium
equilibrium :: Pair Physics -> Vector Momentum
equilibrium ps = equilibrium1 (mass <$> ps) <$> sequence (vcty <$> ps)
    where
        equilibrium1 :: Pair Mass -> Pair Speed -> Momentum
        -- if momentum transferred is t, at equilibrium:
        --    => v1 - t/m1 = v2 + t/m2
        --    => m1*m2*v1 - m2*t = m1*m2*v2 + m1*t
        --    => m1 * m2 * (v1 - v2) = t * (m1 + m2)
        --    => t = (v1 - v2) * m1 * m2 / (m1 + m2)
        --         = (v1 - v2) * 1 / (1/m2 + 1/m1)
        equilibrium1 ms vs = pair (-) vs &* (1 / pair (+) (divPosInf 1 <$> ms))

-- | Transfer Momentum between two Mass/Velocity pairs
transfer :: Pair Mass -> Vector Momentum -> Pair Velocity -> Pair Velocity
transfer ms = sequence .$ liftA2 transfer1 .^ sequence
    where
        transfer1 :: Momentum -> Pair Speed -> Pair Speed
        transfer1 t = liftA2 (+) $ divIndfUnit <$> Pair (negate t) t <*> ms
