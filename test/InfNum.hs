module InfNum (test) where

import Prelude ()
import Util.Prelewd

import Test.Framework
import Test.Framework.TH
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

instance Arbitrary a => Arbitrary (InfNum a) where
    arbitrary = maybe Infinite Numb <$> arbitrary

test :: Test
test = $(testGroupGenerator)

prop_numbord :: (Integer, Integer) -> Bool
prop_numbord (x, y) = compare x y == (compare `on` Numb) x y

prop_eqreflex :: Integer -> Bool
prop_eqreflex x = Numb x == Numb x

prop_infMax :: Integer -> Bool
prop_infMax x =  Infinite > Numb x
              && Numb x < Infinite
