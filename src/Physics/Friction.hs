{-# LANGUAGE NoImplicitPrelude
           #-}
module Physics.Friction ( friction
                        ) where

import Prelewd

import Storage.Pair
import Subset.Num

import Game.Physics
import Game.Vector
import Physics.Types
import Util.Unit

fromDouble :: Double -> Unit t PhysicsValue
fromDouble d = let precision = 100
               in Unit $ ((/) `on` fromInteger) (round $ fromInteger precision * d) precision

friction :: Vector (Maybe (Nonnegative Momentum)) -> Pair Physics -> Pair Physics
friction collides objs = let
            fric = muTransfer collides objs
            equal = equilibrium objs
        in transfer (transfer1 <$> fric <*> equal) objs
    where
        transfer1 :: Momentum -> Momentum -> Momentum
        transfer1 0 _ = 0
        transfer1 _ 0 = 0
        transfer1 x y = if' (signum x == signum y) (minBy (compare `on` abs) y) x

-- | Momentum to transfer due to friction
muTransfer :: Vector (Maybe (Nonnegative Momentum)) -> Pair Physics -> Vector Momentum
muTransfer collides (Pair obj collidee) = let
            normMagnitude = fromDouble $ magnitude $ realToFrac . (<?> 0) <$>  collides
            maxTransfer :: Momentum
            maxTransfer = mu obj * mu collidee &* normMagnitude
            direction = normalize $ setZero <$> collides <*> ((-) `on` vcty) obj collidee
        in (maxTransfer *&) . fromDouble <$> direction
    where
        -- | If mx contains a value, 0. Otherwise, y.
        setZero mx y = mx <&> (\_-> 0) <?> realToFrac y
