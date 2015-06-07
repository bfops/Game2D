{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , FlexibleInstances
           , TemplateHaskell
           #-}
module Util.Relative ( Relative
                     , to
                     , from
                     , test
                     ) where

import Summit.Prelewd
import Summit.Test

import Text.Show

newtype Relative a = Relative a
  deriving (Show, Eq, Ord, Num)

instance Functor f => Sequential Relative f a where
  sequence (Relative a) = Relative <$> a

instance Arbitrary a => Arbitrary (Relative a) where
  arbitrary = Relative <$> arbitrary

infix 6 `to`
infixr 5 `from`

to :: Num a => a -> a -> Relative a
to x y = Relative $ y - x

from :: Num a => Relative a -> a -> a
from (Relative d) !a = d + a

test :: Test
test = $(testGroupGenerator)

prop_id :: (Integer, Integer) -> Result
prop_id (x, y) = (x `to` y `from` x) ==? y

prop_froms :: (Integer, Relative Integer, Relative Integer) -> Result
prop_froms (a, d1, d2) = (d1 `from` d2 `from` a) ==? (d1 + d2 `from` a)
