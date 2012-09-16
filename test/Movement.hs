module Movement (test) where

import Util.Prelewd hiding (empty)

import Data.List
import Data.Map

import Text.Show

import Game.Movement
import Game.Object
import Game.ObjectGroup
import Game.Physics

import Test.HUnit hiding (Test, test)
import Test.QuickCheck
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

test :: Test
test = $(testGroupGenerator)

prop_emptyMove :: (Physics, Position) -> Bool
prop_emptyMove (p, shift) = move shift (KeyPair 0 p) [] == (shift, empty)
