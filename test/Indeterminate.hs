module Indeterminate (test) where

import Prelude ()
import Util.Prelewd

import Test.Framework
import Test.Framework.TH
import Test.HUnit hiding (Test, test)
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

test :: Test
test = $(testGroupGenerator)

prop_numbord :: (Integer, Integer) -> Bool
prop_numbord (x, y) = compare x y == (compare `on` Finite) x y

case_infeq :: Assertion
case_infeq = Infinite @=? (Infinite :: Indeterminate Integer)

prop_eqreflex :: Integer -> Bool
prop_eqreflex x = Finite x == Finite x

prop_infMax :: Integer -> Bool
prop_infMax x =  Infinite > Finite x
              && Finite x < Infinite
