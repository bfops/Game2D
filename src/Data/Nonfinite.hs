{-# LANGUAGE DeriveFunctor #-}
module Data.Nonfinite ( Nonfinite (..)
                      ) where

import Control.Applicative
import Test.QuickCheck

data Nonfinite a = Finite a | Infinite
  deriving (Show, Eq, Functor)

instance Applicative Nonfinite where
  pure = Finite
  Finite f <*> Finite x = Finite (f x)
  _ <*> _ = Infinite

instance Ord a => Ord (Nonfinite a) where
  compare Infinite Infinite = EQ
  compare (Finite _) Infinite = LT
  compare Infinite (Finite _) = GT
  compare (Finite x) (Finite y) = compare x y

instance Arbitrary a => Arbitrary (Nonfinite a) where
  arbitrary = maybe Infinite Finite <$> arbitrary
