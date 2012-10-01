-- | For dealing with continuous ranges
module Util.Range ( Range
                  , empty
                  , range
                  , start
                  , end
                  ) where

import Util.Prelewd hiding (empty)

import Data.Tuple

import Test.QuickCheck
import Text.Show

import Util.Impure

-- | Continuous range
-- Monoid instances selects the overlap of both ranges
newtype Range a = Range (Maybe (Indeterminate a, Indeterminate a))
    deriving (Eq, Show)

instance Ord a => Monoid (Range a) where
    mempty = Range $ Just (Infinite, Infinite)
    mappend (Range mr1) (Range mr2) = Range $ do r1 <- mr1
                                                 r2 <- mr2
                                                 overlapExtant r1 r2
        where
            -- Overlap nonempty ranges
            overlapExtant r1 r2 = assert (validRange r1 && validRange r2)
                                $ cast validRange ((onBoth max `on` fst) r1 r2, (onBoth min `on` snd) r1 r2)

instance (Arbitrary a, Ord a) => Arbitrary (Range a) where
    arbitrary = maybe empty (\(x, y) -> range (min x y) (max x y)) <$> arbitrary

validRange :: Ord a => (Indeterminate a, Indeterminate a) -> Bool
validRange (t1, t2) = liftA2 (>=) t1 t2 /= pure True

-- | Range with nothing in it
empty :: Range a
empty = Range Nothing

-- | Create a range out of its endpoints
range :: Ord a => Indeterminate a -> Indeterminate a -> Range a
range x y = Range $ cast validRange (x, y)

-- | Get the beginning of the range.
-- A return value of Nothing indicates an empty range,
-- and a return value of Just Infinite indicate the range begins at negative infinity
start :: Range a -> Maybe (Indeterminate a)
start (Range r) = fst <$> r

-- | Get the end of the range.
-- A return value of Nothing indicates an empty range,
-- and a return value of Just Infinite indicate the range begins at negative infinity
end :: Range a -> Maybe (Indeterminate a)
end (Range r) = snd <$> r
