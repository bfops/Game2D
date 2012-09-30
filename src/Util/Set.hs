module Util.Set ( Set ()
                , set
                , difference
                , findMin
                , findMax
                ) where

import Data.Set hiding (foldr, foldl, toList)

import Util.Prelewd

set :: (Foldable t, Ord a) => t a -> Set a
set = fromList . toList
