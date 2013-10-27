{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
module Physics.Friction ( friction
                        , test
                        ) where

import Summit.Data.Pair
import Summit.Prelewd
import Summit.Subset.Num
import Summit.Test

import Data.Tuple

import Game.Physics
import Game.Vector hiding (test)
import Physics.Types
import Util.Unit

fromDouble :: Double -> Unit t PhysicsValue
fromDouble d = let precision = 100
               in point $ ((/) `on` fromInteger) (round $ fromInteger precision * d) precision

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

test :: Test
test = $(testGroupGenerator)

type Relative = Nonnegative

setMu :: Mu -> Physics -> Physics
setMu u = mu' (\_-> u)

checkFriction :: (Relative Speed -> Relative Speed -> Bool)
              -> Nonnegative Momentum
              -> Pair (Mu, Physics)
              -> Property
checkFriction check t params = ((<= 1000) . mass . snd) `any` params
                             ==> and
                             $ component' Width (\_->True)
                             $ inner <*> friction (singleV Nothing Width $ Just t)
                             $ uncurry setMu <$> params
    where
        inner = liftA2 check `on` (liftA2 diff `pair`) . map vcty

prop_frictionless :: (Nonnegative Momentum, Pair (Mu, Physics), Bool) -> Property
prop_frictionless (t, (Pair (_, p1) p2),  True) = checkFriction (==) t $ Pair (0, p1) p2
prop_frictionless (t, (Pair p1 (_, p2)), False) = checkFriction (==) t $ Pair p1 (0, p2)

prop_slowDown :: (Nonnegative Momentum, Pair (Positive Mu, Physics)) -> Property
prop_slowDown (t, p) = checkFriction (>=) t $ map2 fromPos <$> p

prop_speedUp :: (Nonnegative Momentum, Pair (Positive Mu, Physics)) -> Property
prop_speedUp (t, Pair (u1, p1) (u2, p2)) = checkFriction (<=) t $ Pair (negate $ fromPos u1, p1) (fromPos u2, p2)
