module Util.Queue ( Queue
                  , enq
                  , front
                  , back
                  , deq
                  , fromList
                  ) where

import Prelude ()
import Util.Prelewd

data Queue a = Queue [a] [a]
    deriving (Eq)

instance Functor Queue where
    fmap f (Queue l1 l2) = Queue (fmap f $ l1 ++ reverse l2) []

instance Foldable Queue where
    foldr f b (Queue l1 l2) = foldr f (foldl (flip f) b l2) l1

instance Traversable Queue where
    sequenceA q = fromList <$> sequenceA (toList q)

instance Monoid (Queue a) where
    mempty = Queue [] []
    mappend q1 q2 = fromList $ on mappend toList q1 q2

enq :: a -> Queue a -> Queue a
enq x (Queue l1 l2) = Queue l1 (x:l2)
    
front :: Queue a -> Maybe a
front (Queue [] l2) = last l2
front (Queue l1 _) = head l1

back :: Queue a -> Maybe a
back (Queue l1 []) = last l1
back (Queue _ l2) = head l2

deq :: Queue a -> Maybe (Queue a)
deq (Queue [] l2) = fromList <$> tail (reverse l2)
deq q = fromList <$> tail (toList q)

fromList :: [a] -> Queue a
fromList l = Queue l []
