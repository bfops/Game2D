{-# LANGUAGE NoImplicitPrelude
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , DeriveFunctor
           #-}
-- | Adds type-checking units for calculations
module Util.Unit ( Unit (..)
                 , UnitMult
                 , (&*)
                 , (*&)
                 , (&/)
                 , (/&)
                 ) where

import Prelewd

import Test.QuickCheck
import Text.Show

infix 9 `Unit`
infixl 6 &*
infixl 6 *&
infixl 6 &/
infixl 6 /&

-- | `Unit t v` is a unit with a unit in `t`, and a value in `v`
newtype Unit t v = Unit { unitless :: v }
    deriving (Show, Eq, Ord, Enum, Num, Real, Fractional, Functor)

instance Applicative f => Sequential (Unit t) f a where
    sequence (Unit v) = pure Unit <*> v

instance Arbitrary v => Arbitrary (Unit t v) where
    arbitrary = Unit <$> arbitrary

-- | Unit multiplication: a x b = c
class UnitMult a b c | a b -> c, a c -> b, b c -> a

-- | Multiply with units
(&*) :: (UnitMult a b c, Num v) => Unit a v -> Unit b v -> Unit c v
(&*) a b = Unit $ unitless a * unitless b

-- | `(&*)` with arguments reversed.
(*&) :: (UnitMult a b c, Num v) => Unit b v -> Unit a v -> Unit c v
(*&) = flip (&*)

-- | Inverse of (*&), (x *& y) &/ y = x
(&/) :: (UnitMult a b c, Fractional v) => Unit c v -> Unit a v -> Unit b v
(&/) c a = Unit $ unitless c / unitless a

-- | Inverse of (&*), (x &* y) /& y = x
(/&) :: (UnitMult a b c, Fractional v) => Unit c v -> Unit b v -> Unit a v
(/&) c b = Unit $ unitless c / unitless b
