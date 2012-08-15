module Range (test) where

import Prelude ()
import Util.Prelewd hiding (empty)

import Util.Fraction
import Util.Range

import Test.Framework
import Test.Framework.TH
import Test.HUnit hiding (Test, test)
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Instances

test :: Test
test = $(testGroupGenerator)

prop_create :: (Indeterminate Integer, Indeterminate Integer) -> Bool
prop_create (s, e) = let s' = min s e
                         e' = max s e
                         rng = range (min s e) (max s e)
                     in fromMaybe (s == e && s /= Infinite) $ endsMatch s' e' <$> start rng <*> end rng
    where
        endsMatch s1 e1 s2 e2 =  s1 == s2
                              && e1 == e2

prop_duality :: Range Integer -> Bool
prop_duality = start <&> ((==) `on` isNothing) <*> end

prop_emptynull :: Range Integer -> Bool
prop_emptynull = (empty ==) . (empty <>)

prop_memptyid :: Range Integer -> Bool
prop_memptyid = (==) <*> (mempty <>)

prop_massoc :: (Range Integer, Range Integer, Range Integer) -> Bool
prop_massoc (x, y, z) = (x <> y) <> z == x <> (y <> z)

prop_mcommute :: (Range Integer, Range Integer) -> Bool
prop_mcommute (x, y) = x <> y == y  <> x
