module Vector (test) where

import Prelude ()
import Util.Prelewd

import Text.Show

import Types

import Test.Framework
import Test.Framework.TH
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

instance Show a => Show (Vector a) where
    show v = "Vector" ++ foldr (\x y -> " " ++ show x ++ y) "" v

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Vector <$> arbitrary <*> arbitrary

test :: Test
test = $(testGroupGenerator)

prop_index :: Vector Integer -> Bool
prop_index v = and $ fmap try [0 .. length (toList v) - 1]
    where
        try :: Integer -> Bool
        try i = (v ! fromIntegral i) == (toList v ! fromIntegral i)
