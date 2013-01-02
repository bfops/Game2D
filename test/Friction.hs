module Friction (test) where

import Prelewd

import Impure

import Control.Arrow
import Data.Tuple
import Storage.Pair
import Subset.Num

import Game.Physics
import Game.Vector
import Physics.Friction
import Physics.Types

import Test.QuickCheck (Property, (==>))
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

test :: Test
test = $(testGroupGenerator)

type Relative = Nonnegative

setMu :: Mu -> Physics -> Physics
setMu u = mu' (\_-> u)

checkFriction :: (Relative Speed -> Relative Speed -> Bool)
              -> Nonnegative Momentum
              -> Pair (Mu, Physics)
              -> Property
checkFriction check t params = (<= 1000).mass.snd `any` params
                             ==> and
                             $ component' Width (\_->True)
                             $ inner <*> friction (singleV Nothing Width $ Just t)
                             $ uncurry setMu <$> params
    where
        inner = liftA2 check `on` withTrace . (liftA2 diff `pair`) . map vcty

prop_frictionless :: (Nonnegative Momentum, Pair (Mu, Physics), Bool) -> Property
prop_frictionless (t, (Pair (_, p1) p2),  True) = checkFriction (==) t $ Pair (0, p1) p2
prop_frictionless (t, (Pair p1 (_, p2)), False) = checkFriction (==) t $ Pair p1 (0, p2)

prop_slowDown :: (Nonnegative Momentum, Pair (Positive Mu, Physics)) -> Property
prop_slowDown (t, p) = checkFriction (>=) t $ first fromPos <$> p

prop_speedUp :: (Nonnegative Momentum, Pair (Positive Mu, Physics)) -> Property
prop_speedUp (t, Pair (u1, p1) (u2, p2)) = checkFriction (<=) t $ Pair (negate $ fromPos u1, p1) (fromPos u2, p2)
