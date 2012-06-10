-- | Common types used in various places
module Util.Types ( Vector (..)
                  , DeltaT
                  ) where

import Prelude ()
import Util.Prelewd

type DeltaT = Double

data Vector a = Vector a a 
    deriving (Eq)

instance Functor Vector where
    fmap f v = pure f <*> v

instance Applicative Vector where
    pure x = Vector x x 
    (Vector fx fy) <*> (Vector x y) = Vector (fx x) (fy y)

instance Foldable Vector where
    foldr f b (Vector x y) = foldr f b [x, y]

instance Traversable Vector where
    sequenceA (Vector x y) = Vector <$> x <*> y
