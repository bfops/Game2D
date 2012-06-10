module Queue (test) where

import Prelude ()
import Util.Prelewd

import Util.Queue

import Test.Framework
import Test.Framework.TH
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

test :: Test
test = $(testGroupGenerator)

-- Conversion between queues and lists
prop_convert :: [Integer] -> Bool
prop_convert l = toList (fromList l) == l

-- Make sure `foldr` works as expected.
prop_foldr :: (Integer, [Integer]) -> Bool
prop_foldr (b, l) = foldr (-) b l == foldr (-) b (fromList l)
