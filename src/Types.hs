-- | Common types used in various places
module Types ( Vector (..)
             , VectorLike (..)
             , setV
             , singleV
             , shorter
             , magnitude
             , dot
             ) where

import Prelude ()
import Util.Prelewd

import Text.Show

import Util.Impure

-- | Homogenous vector
data Vector a = Vector a a 
    deriving (Eq, Show)

instance Real a => Ord (Vector a) where
    compare = (compare :: Double -> Double -> Ordering) `on` magnitude

instance Functor Vector where
    fmap f v = pure f <*> v

instance Applicative Vector where
    pure x = Vector x x 
    (Vector fx fy) <*> (Vector x y) = Vector (fx x) (fy y)

instance Foldable Vector where
    foldr f b (Vector x y) = foldr f b [x, y]

instance Traversable Vector where
    sequenceA (Vector x y) = Vector <$> x <*> y

-- | Constant-size, homogenous types
class VectorLike v where
    vector :: v a -> Vector a

instance VectorLike [] where
    vector (x:y:_) = Vector x y
    vector _ = error "List too small"

-- | Update one index in a vector
setV :: Integer -> a -> Vector a -> Vector a
setV i x v = replaceI <$> v <*> vector [0..]
    where
        -- Replace only the ith index with x
        replaceI = bool x .$ (/= i)

-- | Create a vector with a single nonzero element
singleV :: Num a => Integer -> a -> Vector a
singleV i x = setV i x $ pure 0

-- | Return the shorter of the two vectors
shorter :: (Num a, Ord a) => Vector a -> Vector a -> Vector a
shorter v1 v2 = if on (<) (dot <*> id) v1 v2
                then v1
                else v2

-- | Magnitude of a vector
magnitude :: (Real a, Floating b) => Vector a -> b
magnitude v = sqrt $ realToFrac $ dot v v

-- | Dot product
dot :: Num a => Vector a -> Vector a -> a
dot = foldr (+) 0 $$ liftA2 (*)
