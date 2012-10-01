-- | A Pair is a two-element Set
module Util.Pair ( Pair (..)
                 , tuple
                 ) where

import Util.Prelewd

-- | A pair of something
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (Pair x y) == (Pair a b) = (x == a) && (y == b) || (x == b) && (y == a)

instance Ord a => Ord (Pair a) where
    compare (Pair x y) (Pair a b) = (compare `on` arrange) (x, y) (a, b)
        where
            arrange (q, r) = (min q r, max q r)

-- | Turn a pair into a tuple
tuple :: Pair a -> (a, a)
tuple (Pair x y) = (x, y)
