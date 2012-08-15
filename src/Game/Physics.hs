-- | Functions and data structures for dealing with physical game aspects
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

-- | Coordinate type
type Coord = Milli
-- | Time measurement
type Time = Coord

-- | Object size
type Size = Vector Coord
-- | Object position
type Position = Vector Coord
-- | Object velocity
type Velocity = Vector Coord
-- | Object acceleration
type Acceleration = Vector Coord

-- | Collection of physical properties for an object
data Physics = Physics
        { size  :: Size
        , posn  :: Position
        , vcty  :: Velocity
        , accl  :: Acceleration
        }
    deriving (Show)

-- | Transform size
size' :: (Size -> Size) -> Physics -> Physics
size' f p = p { size = f (size p) }

-- | Transform position
posn' :: (Position -> Position) -> Physics -> Physics
posn' f p = p { posn = f (posn p) }

-- | Transform velocity
vcty' :: (Velocity -> Velocity) -> Physics -> Physics
vcty' f p = p { vcty = f (vcty p) }

-- | Transform acceleration
accl' :: (Acceleration -> Acceleration) -> Physics -> Physics
accl' f p = p { accl = f (accl p) }

-- | Acceleration due to gravity
gravity :: Acceleration
gravity = Vector 0 (-32)

-- | Physical dimensions in the game
data Dimension = Width | Height
    deriving (Eq, Enum, Bounded, Show)

-- | Vector associating each dimension to a vector index
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
    -- | Convert to a vector
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
