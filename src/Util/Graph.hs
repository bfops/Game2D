{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Util.Graph ( Graph
                  , graphData
                  , neighbors
                  , addNode
                  , addEdge
                  , clearNode
                  , clearChildren
                  , clearParents
                  , filterNodes
                  , filterEdges
                  , foldDepth
                  , tree
                  , map
                  , mapWithParents
                  , mapWithEdgesParents
                  , Util.Graph.test
                  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Foldable as Foldable
import Data.Functor
import Data.Hashable
import Data.HashMap.Strict as HashMap
import Data.HashSet as HashSet
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Test.Framework
import Test.Framework.TH
import Test.QuickCheck as QuickCheck
import Text.Show

newtype Graph e n = Graph { _graphData :: HashMap n [(e, n)] }
  deriving (Show)

$(makeLenses ''Graph)

instance (Eq n, Hashable n) => Monoid (Graph e n) where
  mempty = Graph mempty
  mappend (Graph m1) (Graph m2) = Graph $ unionWith (<>) m1 m2

instance (Arbitrary e, Arbitrary n, Eq n, Hashable n) => Arbitrary (Graph e n) where
  arbitrary = do
      nodes <- arbitrary
      len <- arbitrary
      Graph . fromListWith (<>) <$> replicateM len (do
            src <- QuickCheck.elements nodes
            dest <- QuickCheck.elements nodes
            edge <- arbitrary
            return (src, [(edge, dest)]))

instance Foldable (Graph e) where
  foldMap f (Graph m) = foldMap f $ keys m

elem :: (Eq n, Hashable n) => n -> Graph e n -> Bool
elem n (Graph m) = maybe False (\_-> True) $ HashMap.lookup n m

neighbors :: (Eq n, Hashable n) => n -> Graph e n -> Maybe [(e, n)]
neighbors i = HashMap.lookup i . view graphData

addNode :: (Eq n, Hashable n) => n -> Graph e n -> Graph e n
addNode n = over graphData $ HashMap.insert n mempty

-- | Add a directed edge to the Graph. If either node doesn't exist, it will be created.
addEdge :: (Eq n, Hashable n) => n -> n -> e -> Graph e n -> Graph e n
addEdge src dest e
    = over graphData
    $ HashMap.insertWith (<>) src [(e, dest)]
    . HashMap.insertWith (<>) dest mempty

-- | Ensure that the node exists in the Graph with no neighbors.
clearNode :: (Eq n, Hashable n) => n -> Graph e n -> Graph e n
clearNode i = clearParents i . clearChildren i

clearChildren, clearParents :: (Eq n, Hashable n) => n -> Graph e n -> Graph e n
clearChildren i = over graphData $ HashMap.adjust (\_-> mempty) i
clearParents i = over graphData $ HashMap.map $ Prelude.filter ((/= i) . snd)

filterKeys :: (k -> Bool) -> HashMap k v -> HashMap k v
filterKeys f = filterWithKey $ \k _ -> f k

filterNodes :: Hashable n => (n -> Bool) -> Graph e n -> Graph e n
filterNodes p (Graph m) = Graph $ HashMap.map (Prelude.filter (p . snd)) $ filterKeys p m

filterEdges :: Hashable n => (e -> Bool) -> Graph e n -> Graph e n
filterEdges p (Graph m) = Graph $ m <&> Prelude.filter (p . fst)

foldDepth :: (Eq a, Hashable a) => (a -> b -> b) -> b -> a -> Graph e a -> b
foldDepth f b0 a0 (Graph m) = fst $ dfs a0 (b0, mempty)
  where
    dfs a (b, visited) = if HashSet.member a visited
                         then (b, visited)
                         else maybe
                                (b, visited)
                                (Foldable.foldr (dfs . snd) (f a b, HashSet.insert a visited))
                                (HashMap.lookup a m)

-- | `tree n g` returns a tree Graph, rooted at `n`, of the accessible nodes therefrom.
tree :: (Eq n, Hashable n) => n -> Graph e n -> Graph e n
tree n g = let nodes = foldDepth HashSet.insert mempty n g
           in filterNodes (`HashSet.member` nodes) g

mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
mapKeys f = HashMap.fromList . Prelude.map (\(k, v) -> (f k, v)) . HashMap.toList

map :: (Eq n, Eq m, Hashable n, Hashable m) => (n -> m) -> Graph e n -> Graph e m
map f = over graphData $ HashMap.map (Prelude.map $ \(a, n) -> (a, f n)) . mapKeys f

mapWithParents :: (Eq n, Eq m, Hashable n, Hashable m) => (n -> [n] -> m) -> Graph e n -> Graph e m
mapWithParents f = mapWithEdgesParents $ \n -> f n . Prelude.map snd

mapWithEdgesParents :: (Eq n, Eq m, Hashable n, Hashable m) => (n -> [(e, n)] -> m) -> Graph e n -> Graph e m
mapWithEdgesParents f g = let m = fromListWith (<>)
                                $ Foldable.concat
                                $ mapWithKey (\parent s -> s <&> \(e, child) -> (child, [(e, parent)]))
                                $ view graphData g
                          in g <&> \n -> f n $ fromMaybe mempty $ HashMap.lookup n m

test :: Test
test = $(testGroupGenerator)
