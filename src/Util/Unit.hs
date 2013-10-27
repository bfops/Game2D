{-# LANGUAGE NoImplicitPrelude
           , GeneralizedNewtypeDeriving
           , DeriveFunctor
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           #-}
-- | Adds type-checking units for calculations
module Util.Unit ( Unit
                 , UnitMult
                 , unitless
                 , (&*)
                 , (*&)
                 , (&/)
                 , (/&)
                 ) where

import Summit.Impure (undefined)
import Summit.Prelewd
import Summit.Test (Arbitrary (..))

import Text.Show

infix 9 `Unit`
infixl 6 &*
infixl 6 *&
infixl 6 &/
infixl 6 /&

-- | `Unit t v` is a value with a unit of type `t`, and a magnitude of type `v`.
data Unit t v = Unit v t
    deriving (Show, Functor)

instance Pointed (Unit t) v where
    point v = Unit v undefined

instance Eq v => Eq (Unit t v) where
    (==) = (==) `on` unitless

instance Ord v => Ord (Unit t v) where
    compare = compare `on` unitless

instance Num v => Num (Unit t v) where
    fromInteger = point . fromInteger
    (+) = point <$$> ((+) `on` unitless)
    negate = point . negate . unitless
    (*) = point <$$> ((*) `on` unitless)
    signum = point . signum . unitless
    abs = point . abs . unitless

instance Real v => Real (Unit t v) where
    toRational = toRational . unitless

instance Fractional v => Fractional (Unit t v) where
    recip = point . recip . unitless
    fromRational = point . fromRational

instance Applicative f => Sequential (Unit t) f a where
    sequence u = pure point <*> unitless u

instance Arbitrary v => Arbitrary (Unit t v) where
    arbitrary = point <$> arbitrary

unitless :: Unit t v -> v
unitless (Unit v _) = v

-- | Unit multiplication: a x b = c
class UnitMult a b c | a b -> c, a c -> b, b c -> a

-- | Multiply with units
(&*) :: (UnitMult a b c, Num v) => Unit a v -> Unit b v -> Unit c v
(&*) a b = point $ unitless a * unitless b

-- | `(&*)` with arguments reversed.
(*&) :: (UnitMult a b c, Num v) => Unit b v -> Unit a v -> Unit c v
(*&) = flip (&*)

-- | Inverse of (*&), (x *& y) &/ y = x
(&/) :: (UnitMult a b c, Fractional v) => Unit c v -> Unit a v -> Unit b v
(&/) c a = point $ unitless c / unitless a

-- | Inverse of (&*), (x &* y) /& y = x
(/&) :: (UnitMult a b c, Fractional v) => Unit c v -> Unit b v -> Unit a v
(/&) c b = point $ unitless c / unitless b
