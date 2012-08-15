module Game.Physics ( Size
                    , Position
                    , Velocity
                    , Acceleration
                    , Coord
                    , Time
                    , Physics (..)
                    , size'
                    , posn'
                    , vcty'
                    , accl'
                    , gravity
                    , Dimension (..)
                    , dimensions
                    , Vector (..)
                    , VectorLike (..)
                    , setV
                    , singleV
                    , shorter
                    , magnitude
                    , dot
                    ) where

import Prelude ()
import Util.Prelewd

import Data.Fixed
import Text.Show

import Util.Impure

type Coord = Milli
type Time = Coord

type Size = Vector Coord
type Position = Vector Coord
type Velocity = Vector Coord
type Acceleration = Vector Coord

data Physics = Physics
        { size  :: Size
        , posn  :: Position
        , vcty  :: Velocity
        , accl  :: Acceleration
        }
    deriving (Show)

size' :: (Size -> Size) -> Physics -> Physics
size' f p = p { size = f (size p) }

posn' :: (Position -> Position) -> Physics -> Physics
posn' f p = p { posn = f (posn p) }

vcty' :: (Velocity -> Velocity) -> Physics -> Physics
vcty' f p = p { vcty = f (vcty p) }

accl' :: (Acceleration -> Acceleration) -> Physics -> Physics
accl' f p = p { accl = f (accl p) }

gravity :: Acceleration
gravity = Vector 0 (-32)

data Dimension = Width | Height
    deriving (Eq, Enum, Bounded, Show)

dimensions :: Vector Dimension
dimensions = vector [minBound..maxBound]

-- | Homogenous vector
data Vector a = Vector a a 
    deriving (Eq, Show)

instance Real a => Ord (Vector a) where
    compare = (compare :: Double -> Double -> Ordering) `on` magnitude

instance Functor Vector where
    fmap = apmap

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

-- | Set one dimension of a vector
setV :: Dimension -> a -> Vector a -> Vector a
setV d x = liftA2 (\d' -> iff (d == d') x) dimensions

-- | Construct a vector with one element different from the others
singleV :: a -> Dimension -> a -> Vector a
singleV zero d x = setV d x $ pure zero

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
dot = foldr (+) 0 .: liftA2 (*)
