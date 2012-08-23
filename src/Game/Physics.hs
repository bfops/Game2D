-- | Functions and data structures for dealing with physical game aspects
module Game.Physics ( Units (..)
                    , PhysicsValue
                    , Scalar
                    , Mass
                    , Time
                    , Distance
                    , Speed
                    , Acceleration
                    , Size
                    , Position
                    , Velocity
                    , Physics (..)
                    , size'
                    , posn'
                    , vcty'
                    , accl'
                    , dist
                    , speed
                    , accel
                    ) where

import Prelude ()
import Util.Prelewd

import Data.Fixed

import Test.QuickCheck
import Text.Show

import Game.Vector
import Util.Unit

data Units = Mass | Size | Time
    deriving (Show, Eq, Enum, Bounded, Ord)

instance Arbitrary Units where
    arbitrary = elements [minBound..maxBound]

type PhysicsValue = Unit Units Milli
type Scalar = PhysicsValue
type Mass = PhysicsValue
type Time = PhysicsValue
type Distance = PhysicsValue
type Speed = PhysicsValue
type Acceleration = PhysicsValue

type Size = Vector Distance
type Position = Vector Distance
type Velocity = Vector Speed

-- | Collection of physical properties for an object
data Physics = Physics
        { size  :: Size
        , posn  :: Position
        , vcty  :: Velocity
        , accl  :: Vector Acceleration
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
accl' :: (Vector Acceleration -> Vector Acceleration) -> Physics -> Physics
accl' f p = p { accl = f (accl p) }

dist, speed, accel :: (Show a, Fractional a) => a -> Unit Units a
dist d = d `unit` Size
speed v = dist v / 1 `unit` Time
accel a = speed a / 1 `unit` Time

