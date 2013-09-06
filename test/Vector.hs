module Vector (test) where

import Summit.Prelewd
import Summit.Data.Member

import Game.Vector

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

test :: Test
test = $(testGroupGenerator)

prop_dimExists :: Dimension -> Bool
prop_dimExists = (`elem` dimensions)

prop_index :: Vector Integer -> Bool
prop_index v = and $ fmap index $ indices v
    where
        index i = (v ! fromIntegral i) == (toList v ! fromIntegral i)

prop_ordAssoc :: (Vector Integer, Vector Integer, Vector Integer) -> Bool
prop_ordAssoc (x, y, z) = min (min x y) z == min x (min y z)

prop_shortMult :: (Vector Integer, Vector Integer, Integer) -> Bool
prop_shortMult (x, y, c) = fmap (c*) (min x y) == (min `on` fmap (c*)) x y

prop_dotCommute :: (Vector Integer, Vector Integer) -> Bool
prop_dotCommute (x, y) = dot x y == dot y x

prop_dotMult :: (Vector Integer, Vector Integer, Integer) -> Bool
prop_dotMult (v, w, c) = c * dot v w == dot ((c *) <$> v) w

prop_dotAssoc :: (Vector Integer, Vector Integer, Vector Integer) -> Bool
prop_dotAssoc (x, y, z) =  (dot x (y <&> (+) <*> z)) == dot x y + dot x z
                        && (dot (x <&> (+) <*> y) z) == dot x z + dot y z

prop_triangle :: (Vector Double, Vector Double, Vector Double) -> Bool
prop_triangle (x, y, z) = magnitude (x - y) + magnitude (x - z) >= magnitude (y - z)

prop_doubleSet :: (Vector Integer, Dimension, Integer) -> Bool
prop_doubleSet (v, d, x) = let set = setV d x v
                           in set == setV d x set

indices :: Vector a -> [Integer]
indices v = [0 .. length v - 1]
