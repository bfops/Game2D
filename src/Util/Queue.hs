-- | Queue data structure and operations
module Util.Queue ( Queue
                  , enq
                  , front
                  , back
                  , deq
                  , fromList
                  ) where

import Prelude ()
import Util.Prelewd

-- | LIFO data structure
data Queue a = Queue [a] [a]
    deriving (Eq)

instance Functor Queue where
    -- | O(n)
    fmap f = fromList . fmap f . toList

instance Foldable Queue where
    -- | O(n)
    foldr f b (Queue l1 l2) = foldr f (foldl (flip f) b l2) l1

instance Traversable Queue where
    -- | O(n)
    sequenceA q = fromList <$> sequenceA (toList q)

instance Monoid (Queue a) where
    mempty = Queue [] []
    -- | O(n), where `n` is the length of the first queue
    mappend q1 q2 = fromList $ on mappend toList q1 q2

-- | Add an element into the queue
enq :: a -> Queue a -> Queue a
enq x (Queue l1 l2) = Queue l1 (x:l2)

-- | Get the front of the queue
front :: Queue a -> Maybe a
front (Queue [] l2) = last l2
front (Queue l1 _) = head l1

-- | Get the back of the queue
back :: Queue a -> Maybe a
back (Queue l1 []) = last l1
back (Queue _ l2) = head l2

-- | Remove the head of the queue
deq :: Queue a -> Maybe (Queue a)
deq (Queue [] l2) = fromList <$> tail (reverse l2)
deq q = fromList <$> tail (toList q)

-- | Construct a queue from a list
fromList :: [a] -> Queue a
fromList l = Queue l []
