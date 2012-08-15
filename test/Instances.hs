module Instances ( Arbitrary (..)
                 ) where

import Prelude ()
import Util.Prelewd

import Data.Fixed

import Util.Fraction

import Test.QuickCheck hiding (Fixed)

import Game.Physics

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Vector <$> arbitrary <*> arbitrary

instance (Eq a, Num a, Arbitrary a) => Arbitrary (Fraction a) where
    arbitrary = frac .$ nonzeroDenominator <$> arbitrary <*> arbitrary
        where
            nonzeroDenominator t = iff (t == 0) 1 t

instance Arbitrary a => Arbitrary (InfNum a) where
    arbitrary = maybe Infinite Numb <$> arbitrary

instance HasResolution a => Arbitrary (Fixed a) where
    arbitrary = realToFrac <$> (arbitrary :: Gen Double)

instance Arbitrary Physics where
    arbitrary = Physics <$> arbitrarysize <*> arbitrary <*> arbitrary <*> arbitrary
        where
            arbitrarysize :: (Arbitrary a, Num a) => Gen (Vector a)
            arbitrarysize = (abs <$>)<$> arbitrary
