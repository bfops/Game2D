module Physics (test) where

import Prelude ()
import Util.Prelewd

import Text.Show

import Game.Physics

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
