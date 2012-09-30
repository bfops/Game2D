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

delete :: Ord k => k -> Map k v -> Maybe (Map k v)
delete = mcond .$ member $$ Map.delete

join :: Ord k => Map k v -> Map k v -> Map k v
join = union

joinWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
joinWith = unionWith
