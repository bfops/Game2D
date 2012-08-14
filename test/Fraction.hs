module Fraction (test) where

import Prelude ()
import Util.Prelewd

import Util.Fraction

import Test.Framework
import Test.Framework.TH
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

instance (Real a, Arbitrary a) => Arbitrary (Fraction a) where
    arbitrary = frac .$ nonzeroDenominator <$> arbitrary <*> arbitrary
        where
            nonzeroDenominator t = iff (t == 0) 1 t

test :: Test
test = $(testGroupGenerator)

compare' :: (Num a, Ord a) => Fraction a -> Fraction a -> Ordering
compare' = compare

-- Preserve comparison upon casting to Fraction
prop_convertCompare :: (Integer, Integer) -> Bool
prop_convertCompare (x, y) = compare x y == (compare' `on` realToFrac) x y
