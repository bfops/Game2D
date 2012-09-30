module Util.Pair ( Pair
                 , pair
                 , tuple
                 ) where

import Util.Prelewd

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (Pair x y) == (Pair a b) = (x == a) && (y == b) || (x == b) && (y == a)

instance Ord a => Ord (Pair a) where
    compare (Pair x y) (Pair a b) = compare (x,y) (a,b)

pair :: Ord a => a -> a -> Pair a
pair = Pair .$ min $$ max

tuple :: Pair a -> (a, a)
tuple (Pair x y) = (x, y)
