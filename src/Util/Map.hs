-- | Map one set to another
module Util.Map ( Map ()
                , lookup
                , keys
                , assocs
                , insert
                , singleton
                , modify
                , member
                , mapKeys
                , join
                , joinWith
                , Map.filter
                , Util.Map.delete
                , foldrWithKey
                , foldlWithKey
                , fromList
                ) where

import Data.Map as Map

import Util.Prelewd hiding (join)

-- | Change or delete a value
-- Renamed to avoid conflicts
modify :: Ord k => (v -> Maybe v) -> k -> Map k v -> Maybe (Map k v)
modify f = mcond .$ member $$ update f

-- | O(lg(n))
delete :: Ord k => k -> Map k v -> Maybe (Map k v)
delete = mcond .$ member $$ Map.delete

-- | O(n+m)
join :: Ord k => Map k v -> Map k v -> Map k v
join = union

-- | O(n+m)
joinWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
joinWith = unionWith
