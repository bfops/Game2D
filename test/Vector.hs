module Vector (test) where

import Prelude ()
import Util.Prelewd

import Text.Show

import Types

import Test.Framework
import Test.Framework.TH
import Test.QuickCheck hiding (vector)
import Test.Framework.Providers.QuickCheck2

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Vector <$> arbitrary <*> arbitrary

test :: Test
test = $(testGroupGenerator)

indices :: Vector a -> [Integer]
indices v = [0 .. length v - 1]

prop_index :: Vector Integer -> Bool
prop_index v = and $ fmap try $ indices v
    where
        try :: Integer -> Bool
        try i = (v ! fromIntegral i) == (toList v ! fromIntegral i)

prop_set :: Vector Integer -> Bool
prop_set v = and $ fmap updateWorks $ indices v
    where
        -- To make sure it's not the same value
        x i = v!i - 1
        updateWorks i = and $ indexMatches i <$> setV i (x i) v <*> vector [0..]
        indexMatches i k j = (i == j && k == x i) || (i /= j && k == v!j)

prop_single :: Integer -> Bool
prop_single x = and $ fmap isSingle $ indices $ pure 0
    where
        v i = singleV i x
        isSingle i = and $ p i <$> vector [0..] <*> v i
        p i t a = (i == t && a == x) || (i /= t && a == 0)

prop_shortAssoc :: (Vector Integer, Vector Integer, Vector Integer) -> Bool
prop_shortAssoc (x, y, z) = shorter (shorter x y) z == shorter x (shorter y z)

prop_shortMult :: (Vector Integer, Vector Integer, Integer) -> Property
prop_shortMult (x, y, c) = c >= 0 ==> ((c*) <$> shorter x y) == shorter ((c*) <$> x) ((c*) <$> y)

prop_dotCommute :: (Vector Integer, Vector Integer) -> Bool
prop_dotCommute (x, y) = dot x y == dot y x

prop_dotMult :: (Vector Integer, Vector Integer, Integer) -> Bool
prop_dotMult (v, w, c) = c * dot v w == dot ((c *) <$> v) w

prop_dotAssoc :: (Vector Integer, Vector Integer, Vector Integer) -> Bool
prop_dotAssoc (x, y, z) =  (dot x (y <&> (+) <*> z)) == dot x y + dot x z
                        && (dot (x <&> (+) <*> y) z) == dot x z + dot y z

prop_magnitude :: Vector Integer -> Bool
prop_magnitude v = let c = (-2) :: Integer
                   in (realToFrac (abs c) * magnitude v :: Double) == magnitude ((c*) <$> v)
