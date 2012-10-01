module Movement (test) where

import Util.Prelewd

import Data.List

import Game.Movement
import Game.Object
import Game.Physics
import Util.Map

import Test.HUnit hiding (Test, test)
import Test.QuickCheck
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

test :: Test
test = $(testGroupGenerator)

prop_emptyMove :: (Physics, Position) -> Bool
prop_emptyMove (p, shift) = move shift 0 p mempty == (shift, mempty)
