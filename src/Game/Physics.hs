-- | Functions and data structures for dealing with physical game aspects
module Game.Physics ( Size
                    , Position
                    , Velocity
                    , Acceleration
                    , Distance
                    , Speed
                    , ScalarAccel
                    , Coord
                    , Time
                    , DeltaP
                    , DeltaV
                    , Physics (..)
                    , size'
                    , posn'
                    , vcty'
                    , accl'
                    ) where

import Prelude ()
import Util.Prelewd

import Data.Fixed

import Test.QuickCheck
import Text.Show

import Game.Vector

-- | Coordinate type
type Coord = Milli
-- | Time measurement
type Time = Coord

newtype Distance = Dist Coord
    deriving (Eq, Show, Num, Ord, Real, Fractional, Arbitrary)
newtype Speed = Speed Coord
    deriving (Eq, Show, Num, Ord, Real, Fractional, Arbitrary)
newtype ScalarAccel = SAccel Coord
    deriving (Eq, Show, Num, Ord, Real, Fractional, Arbitrary)

type Size = Vector Distance
type Position = Vector Distance
type Velocity = Vector Speed
type Acceleration = Vector ScalarAccel

type DeltaP = Vector Distance
type DeltaV = Vector Speed

-- | Collection of physical properties for an object
data Physics = Physics
        { size  :: Size
        , posn  :: Position
        , vcty  :: Velocity
        , accl  :: Acceleration
        }
    deriving (Show)

instance Arbitrary Physics where
    arbitrary = Physics <$> (abs <$> arbitrary) <*> arbitrary <*> arbitrary <*> arbitrary

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
