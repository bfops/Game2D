module Vector (test) where

import Prelude ()
import Util.Prelewd

import Text.Show

import Types

import Test.Framework
import Test.Framework.TH
import Test.QuickCheck hiding (vector)
import Test.Framework.Providers.QuickCheck2

instance Show a => Show (Vector a) where
    show v = "Vector" ++ foldr (\x y -> " " ++ show x ++ y) "" v

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
        updateWorks i = (setV i (x i) v) ! i == x i

prop_single :: Integer -> Bool
prop_single x = and $ fmap isSingle $ indices $ pure 0
    where
        v i = singleV i x
        isSingle i = and $ p i <$> vector (indices $ v i) <*> v i
        p i t a = (i == t && a == x) || (i /= t && a == 0)

prop_short :: (Vector Integer, Vector Integer, Vector Integer) -> Bool
prop_short (x, y, z) = shorter (shorter x y) z == shorter x (shorter y z)

prop_dot :: (Vector Integer, Vector Integer, Vector Integer) -> Bool
prop_dot (x, y, z) =  (dot x (y <&> (+) <*> z)) == dot x y + dot x z
                   && (dot (x <&> (+) <*> y) z) == dot x z + dot y z

prop_magnitude :: Vector Integer -> Bool
prop_magnitude v = let c = (-2) :: Integer
                   in (realToFrac (abs c) * magnitude v :: Double) == magnitude ((c*) <$> v)
