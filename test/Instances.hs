module Instances ( Arbitrary (..)
                 ) where

import Prelude ()
import Util.Prelewd hiding (empty)

import Data.Fixed

import Game.Physics
import Util.Fraction
import Util.Range

import Test.QuickCheck hiding (Fixed)

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Vector <$> arbitrary <*> arbitrary

instance (Eq a, Num a, Arbitrary a) => Arbitrary (Fraction a) where
    arbitrary = frac .$ nonzeroDenominator <$> arbitrary <*> arbitrary
        where
            nonzeroDenominator t = iff (t == 0) 1 t

instance Arbitrary a => Arbitrary (Indeterminate a) where
    arbitrary = maybe Infinite Finite <$> arbitrary

instance HasResolution a => Arbitrary (Fixed a) where
    arbitrary = realToFrac <$> (arbitrary :: Gen Double)

instance Arbitrary Physics where
    arbitrary = Physics <$> arbitrarysize <*> arbitrary <*> arbitrary <*> arbitrary
        where
            arbitrarysize :: (Arbitrary a, Num a) => Gen (Vector a)
            arbitrarysize = (abs <$>)<$> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Range a) where
    arbitrary = maybe empty (\(x, y) -> range (min x y) (max x y)) <$> arbitrary
