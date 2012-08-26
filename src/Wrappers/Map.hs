-- | Wrap of Data.Map, mainly for fewer name collisions
module Wrappers.Map ( module M
                    ) where

import Data.Map as M hiding (foldr, foldl, size, toList)
