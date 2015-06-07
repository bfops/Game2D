{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Data.Pair( Pair (..)
                , pair
                ) where

import Data.Foldable

data Pair a = Pair a a
  deriving (Show, Eq, Ord, Functor, Foldable)

pair :: (a -> a -> b) -> Pair a -> b
pair f (Pair x y) = f x y
{-# INLINE pair #-}
