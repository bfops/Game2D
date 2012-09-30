module Util.Member ( Member (..)
                   ) where

import Data.List as List (elem)
import Data.Map (Map)

import Util.Prelewd

class (Foldable t, Eq a) => Member t a where
    elem :: a -> t a -> Bool
    elem x = foldr (\y b -> b || x == y) False

instance Eq a => Member [] a where
    elem = List.elem

instance (Ord k, Eq v) => Member (Map k) v
