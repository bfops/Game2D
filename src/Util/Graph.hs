{-# LANGUAGE NoImplicitPrelude
           , FlexibleInstances
           , MultiParamTypeClasses
           , TemplateHaskell
           #-}
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
                  , mapWithParents
                  , mapWithEdgesParents
                  , Util.Graph.test
                  ) where

import Summit.Data.Map as M hiding (filter)
import Summit.Data.Set as S
import Summit.Data.Member
import Summit.Prelewd
import Summit.Test

import Data.Tuple
import Text.Show

newtype Graph e n = Graph (Map n [(e, n)])
  deriving (Show)

instance (Ord n, Ord m) => Mappable (->) (Graph e) n m where
  map f = graphData' $ map2 f >>> map (map (map f))

instance Ord n => Monoid (Graph e n) where
  mempty = Graph mempty
  mappend (Graph m1) (Graph m2) = Graph $ unionWith (<>) m1 m2

instance (Arbitrary e, Arbitrary n, Ord n) => Arbitrary (Graph e n) where
  arbitrary = do
    nodes <- arbitrary
    Graph . fromListWith (<>) <$> listOf (do
              src <- elements nodes
              dest <- elements nodes
              edge <- arbitrary
              return (src, [(edge, dest)]))

instance Foldable (Graph e) where
  foldMap f (Graph m) = foldMap f $ keys m

instance Ord n => Member (Graph e) n where
  elem n (Graph m) = lookup n m $> True <?> False

graphData :: Graph e n -> Map n [(e, n)]
graphData (Graph m) = m

graphData' :: (Map n [(e, n)] -> Map m [(f, m)]) -> Graph e n -> Graph f m
graphData' f (Graph m) = Graph $ f m

neighbors :: Ord n => n -> Graph e n -> Maybe [(e, n)]
neighbors i = graphData >>> lookup i

addNode :: Ord n => n -> Graph e n -> Graph e n
addNode n = graphData' $ M.insert n mempty

-- | Add a directed edge to the Graph. If either node doesn't exist, it will be created.
addEdge :: Ord n => n -> n -> e -> Graph e n -> Graph e n
addEdge src dest e = graphData' $ alter (\m -> Just $ (e, dest) : (m <?> mempty)) src
                                . alter (<|> Just mempty) dest

-- | Ensure that the node exists in the Graph with no neighbors.
clearNode :: Ord n => n -> Graph e n -> Graph e n
clearNode i = clearChildren i >>> clearParents i

clearChildren, clearParents :: Ord n => n -> Graph e n -> Graph e n
clearChildren i = graphData' $ alter (\_-> Just mempty) i
clearParents i = graphData' $ map $ filter $ snd >>> (/= i)

filterNodes :: Ord n => (n -> Bool) -> Graph e n -> Graph e n
filterNodes p (Graph m) = Graph $ filterKeys p m <&> filter (p . snd)

filterEdges :: Ord n => (e -> Bool) -> Graph e n -> Graph e n
filterEdges p (Graph m) = Graph $ m <&> filter (p . fst)

foldDepth :: Ord a => (a -> b -> b) -> b -> a -> Graph e a -> b
foldDepth f b0 a0 (Graph m) = fst $ dfs a0 (b0, mempty)
  where
    dfs a (b, visited) = if elem a visited
                         then (b, visited)
                         else foldr (dfs . snd) (f a b, S.insert a visited)
                          <$> lookup a m
                          <?> (b, visited)

-- | `tree n g` returns a tree Graph, rooted at `n`, of the accessible nodes therefrom.
tree :: Ord n => n -> Graph e n -> Graph e n
tree n g = let nodes = foldDepth S.insert mempty n g
           in filterNodes (`elem` nodes) g

mapWithParents :: (Ord n, Ord m) => (n -> [n] -> m) -> Graph e n -> Graph e m
mapWithParents f = mapWithEdgesParents $ \n -> f n . map snd

mapWithEdgesParents :: (Ord n, Ord m) => (n -> [(e, n)] -> m) -> Graph e n -> Graph e m
mapWithEdgesParents f g = let m = graphData g
                                $>> mapWithKey (\parent s -> toList s <&> \(e, child) -> (child, [(e, parent)]))
                                >>> concat
                                >>> fromListWith (<>)
                          in g <&> \n -> f n $ lookup n m <?> mempty

test :: Test
test = $(testGroupGenerator)
