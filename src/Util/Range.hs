{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
-- | For dealing with continuous ranges
module Util.Range ( Range
                  , empty
                  , range
                  , start
                  , end
                  , test
                  ) where

import Summit.Impure
import Summit.Num.Nonfinite
import Summit.Prelewd hiding (empty)
import Summit.Test hiding (assert)

import Data.Maybe (isNothing)
import Data.Tuple
import Text.Show

(>>==) :: Monad m => (m a, m b) -> (a -> b -> m c) -> m c
(>>==) (x, y) f = x >>= \a -> f a =<< y

-- | Continuous range
-- Monoid instances selects the overlap of both ranges
newtype Range a = Range (Maybe (Nonfinite a, Nonfinite a))
    deriving (Eq, Show)

instance Ord a => Monoid (Range a) where
    mempty = Range $ Just (Infinite, Infinite)
    mappend (Range mr1) (Range mr2) = Range $ (mr1, mr2) >>== overlapExtant
        where
            -- Overlap nonempty ranges
            overlapExtant r1 r2 = assert (validRange r1 && validRange r2)
                                $ cast validRange ((onBoth max `on` fst) r1 r2, (onBoth min `on` snd) r1 r2)

instance (Arbitrary a, Ord a) => Arbitrary (Range a) where
    arbitrary = (\(x, y) -> range (min x y) (max x y)) <$$> arbitrary <&> (<?> empty)

validRange :: Ord a => (Nonfinite a, Nonfinite a) -> Bool
validRange (t1, t2) = liftA2 (>) t1 t2 /= pure True

-- | Range with nothing in it
empty :: Range a
empty = Range Nothing

-- | Create a range out of its endpoints
range :: Ord a => Nonfinite a -> Nonfinite a -> Range a
range x y = Range $ cast validRange (x, y)

-- | Get the beginning of the range.
-- A return value of Nothing indicates an empty range,
-- and a return value of Just Infinite indicate the range begins at negative infinity
start :: Range a -> Maybe (Nonfinite a)
start (Range r) = fst <$> r

-- | Get the end of the range.
-- A return value of Nothing indicates an empty range,
-- and a return value of Just Infinite indicate the range begins at negative infinity
end :: Range a -> Maybe (Nonfinite a)
end (Range r) = snd <$> r

test :: Test
test = $(testGroupGenerator)

prop_create :: (Nonfinite Integer, Nonfinite Integer) -> Bool
prop_create (s, e) = let s' = min s e
                         e' = max s e
                         rng = range (min s e) (max s e)
                     in (endsMatch s' e' <$> start rng <*> end rng) <?> (s == e && s /= Infinite)
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
prop_mcommute (x, y) = x <> y == y <> x
