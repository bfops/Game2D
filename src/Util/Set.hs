-- | Set of things, independent of order
module Util.Set ( Set ()
                , set
                , difference
                , findMin
                , findMax
                ) where

import Data.Set hiding (foldr, foldl, toList)

import Util.Prelewd

-- | Create a set from something foldable
set :: (Foldable t, Ord a) => t a -> Set a
set = fromList . toList
