module Math (test) where

import Prelewd

import Physics.Types

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

test :: Test
test = $(testGroupGenerator)

prop_reflex :: PhysicsValue -> Bool
prop_reflex x = x == x

prop_zeroIdentity :: PhysicsValue -> Bool
prop_zeroIdentity x = x + 0 == x

prop_negInverse :: PhysicsValue -> Bool
prop_negInverse x = x - x == 0

prop_addAssoc :: (PhysicsValue, PhysicsValue, PhysicsValue) -> Bool
prop_addAssoc (x, y, z) = x + (y + z) == (x + y) + z

prop_addCommute :: (PhysicsValue, PhysicsValue) -> Bool
prop_addCommute (x, y) = x + y == y + x

prop_multZero :: PhysicsValue -> Bool
prop_multZero x = x * 0 == 0

prop_multIdentity :: PhysicsValue -> Bool
prop_multIdentity x = x * 1 == x

prop_multCommute :: (PhysicsValue, PhysicsValue) -> Bool
prop_multCommute (x, y) = x * y == y * x
