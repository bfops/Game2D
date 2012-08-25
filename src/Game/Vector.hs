module Game.Vector ( Vector
                   , Dimension (..)
                   , dimensions
                   , setV
                   , singleV
                   , vector
                   , magnitude
                   , normalize
                   , dot
                   , component
                   ) where

import Prelude ()
import Util.Prelewd

import Data.Tuple

import Test.QuickCheck hiding (vector)
import Text.Show

-- | Physical dimensions in the game
data Dimension = Width | Height
    deriving (Eq, Show, Enum, Bounded)

instance Arbitrary Dimension where
    arbitrary = elements [minBound..maxBound]

-- | Vector associating each dimension to a vector component
dimensions :: Vector Dimension
dimensions = Vector Width Height

-- | Homogenous vector
data Vector a = Vector a a 
    deriving (Eq, Show)

instance Num a => Num (Vector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (Vector a) where
    recip = fmap recip
    fromRational = pure . fromRational

instance Real a => Ord (Vector a) where
    compare = compare `on` (dot <*> id)

instance Functor Vector where
    fmap = apmap

instance Applicative Vector where
    pure x = Vector x x 
    (Vector fx fy) <*> (Vector x y) = Vector (fx x) (fy y)

instance Foldable Vector where
    foldr f b (Vector x y) = foldr f b [x, y]

instance Traversable Vector where
    sequenceA (Vector x y) = Vector <$> x <*> y

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = sequence $ pure arbitrary

component :: Dimension -> Vector a -> a
component d = fromJust . foldr (\(d', x) a -> a <|> mcond (d == d') x) Nothing . liftA2 (,) dimensions

-- | Set one dimension of a vector
setV :: Dimension -> a -> Vector a -> Vector a
setV d x = liftA2 (\d' -> iff (d == d') x) dimensions

-- | Construct a vector with one element different from the others
singleV :: a -> Dimension -> a -> Vector a
singleV zero d x = setV d x $ pure zero

vector :: a -> [(Dimension, a)] -> Vector a
vector = foldr (uncurry setV) . pure

-- | Magnitude of a vector
magnitude :: (Real a, Floating b) => Vector a -> b
magnitude v = sqrt $ realToFrac $ dot v v

normalize :: (Eq a, Fractional a) => Vector a -> Vector a
normalize 0 = 0
normalize v = (/ dot v v) <$> v

-- | Dot product
dot :: Num a => Vector a -> Vector a -> a
dot = sum .$ (*)
