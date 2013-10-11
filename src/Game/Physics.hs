{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
module Game.Physics ( Physics (..)
                    , Collisions
                    , size'
                    , mass'
                    , posn'
                    , vcty'
                    , accl'
                    , mu'
                    , transfer
                    , equilibrium
                    , inelastic
                    ) where

import Summit.Impure
import Summit.Data.Pair
import Summit.Data.Set
import Summit.Num.Nonfinite
import Summit.Prelewd
import Summit.Subset.Num
import Summit.Template.MemberTransformer
import Summit.Test (Arbitrary (..))

import Text.Show

import Game.Vector
import Physics.Types
import Util.Unit

divIndfUnit :: (Ord a, Fractional a, UnitMult x y z) => Unit z a -> Nonfinite (Positive (Unit y a)) -> Unit x a
divIndfUnit z x = mapInfinite ((z /&) . fromPos <$> x) 0

-- | Collection of physical properties for an object
data Physics = Physics
        { size :: Size
        , mass :: Mass
        , posn :: Position
        , vcty :: Velocity
        , accl :: Vector Acceleration
        , mu   :: Mu
        }
    deriving (Show, Eq)

type Collisions = Set Dimension

$(memberTransformers ''Physics)

-- | Transform Velocities of two Physics objects.
vcty2' :: (Pair Velocity -> Pair Velocity) -> Pair Physics -> Pair Physics
vcty2' f ps = vcty' . (\x _->x) <$> f (vcty <$> ps) <*> ps

instance Arbitrary Physics where
    -- Anything with Infinite mass isn't allowed to accelerate.
    arbitrary = do m <- arbitrary
                   Physics <$> arbitrary <*> pure m <*> arbitrary <*> arbitrary <*> inner m <*> arbitrary
        where
            inner Infinite = pure 0
            inner _ = arbitrary

-- | Transfer Momentum between two objects.
transfer :: Vector Momentum -> Pair Physics -> Pair Physics
transfer toTransfer ps = vcty2' (sequence . liftA2 transfer1 toTransfer . sequence) ps
    where
        transfer1 :: Momentum -> Pair Speed -> Pair Speed
        transfer1 t = liftA2 (+) $ divIndfUnit <$> Pair (negate t) t <*> (mass <$> ps)

-- | Determine the momentum transfer required to reach velocity equilibrium.
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
        equilibrium1 ms vs = pair (-) vs &* fromPos (pair massFactor ms)

        massFactor = (`mapInfinite` error "Can't solve Infinite equilibrium")
                <$$> onBoth (\m1 m2 -> m1 * m2 / (m1 + m2))

inelastic :: Pair Physics -> Pair Physics
inelastic = transfer =<< equilibrium
