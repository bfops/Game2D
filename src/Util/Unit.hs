-- | Adds type-checking units for calculations
module Util.Unit ( Unit (..)
                 , UnitMult
                 , (&*)
                 ) where

import Prelewd

import Test.QuickCheck
import Text.Show

infix 9 `Unit`

-- | `Unit t v` is a unit with a unit in `t`, and a value in `v`
newtype Unit t v = Unit { unitless :: v }
    deriving (Show, Eq, Ord, Enum, Num, Real, Fractional, Functor)

instance Arbitrary v => Arbitrary (Unit t v) where
    arbitrary = Unit <$> arbitrary

class UnitMult a b c | a b -> c, a c -> b, b c -> a

(&*) :: (UnitMult a b c, Num v) => Unit a v -> Unit b v -> Unit c v
(&*) a b = Unit $ unitless a * unitless b
