{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Adds type-checking units for calculations
module Util.Unit ( Unit
                 , UnitMult
                 , unitless
                 , (&*)
                 , (*&)
                 , (&/)
                 , (/&)
                 ) where

import Control.Applicative
import Data.Function
import Data.Traversable
import Test.QuickCheck (Arbitrary (..))
import Text.Show

infix 9 `Unit`
infixl 6 &*
infixl 6 *&
infixl 6 &/
infixl 6 /&

-- | `Unit t v` is a value with a unit of type `t`, and a magnitude of type `v`.
data Unit t v = Unit v
    deriving (Show, Functor)

instance Applicative (Unit t) where
    pure = Unit
    Unit f <*> Unit v = Unit (f v)

instance Eq v => Eq (Unit t v) where
    (==) = (==) `on` unitless

instance Ord v => Ord (Unit t v) where
    compare = compare `on` unitless

instance Num v => Num (Unit t v) where
    fromInteger = pure . fromInteger
    x + y = pure $ ((+) `on` unitless) x y
    negate = pure . negate . unitless
    (*) = pure <$$> ((*) `on` unitless)
    signum = pure . signum . unitless
    abs = pure . abs . unitless

instance Real v => Real (Unit t v) where
    toRational = toRational . unitless

instance Fractional v => Fractional (Unit t v) where
    recip = pure . recip . unitless
    fromRational = pure . fromRational

instance Traversable (Unit t) where
    sequenceA u = pure pure <*> unitless u

instance Arbitrary v => Arbitrary (Unit t v) where
    arbitrary = pure <$> arbitrary

unitless :: Unit t v -> v
unitless (Unit v) = v

-- | Unit multiplication: a x b = c
class UnitMult a b c | a b -> c, a c -> b, b c -> a

-- | Multiply with units
(&*) :: (UnitMult a b c, Num v) => Unit a v -> Unit b v -> Unit c v
(&*) a b = pure $ unitless a * unitless b

-- | `(&*)` with arguments reversed.
(*&) :: (UnitMult a b c, Num v) => Unit b v -> Unit a v -> Unit c v
(*&) = flip (&*)

-- | Inverse of (*&), (x *& y) &/ y = x
(&/) :: (UnitMult a b c, Fractional v) => Unit c v -> Unit a v -> Unit b v
(&/) c a = pure $ unitless c / unitless a

-- | Inverse of (&*), (x &* y) /& y = x
(/&) :: (UnitMult a b c, Fractional v) => Unit c v -> Unit b v -> Unit a v
(/&) c b = pure $ unitless c / unitless b
