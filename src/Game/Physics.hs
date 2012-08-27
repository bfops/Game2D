-- | Functions and data structures for dealing with physical game aspects
module Game.Physics ( PhysicsValue
                    , Scalar
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
                    , time
                    , fromTime
                    , dist
                    , fromDist
                    , speed
                    , fromSpeed
                    , accel
                    , fromAccel
                    ) where

import Util.Prelewd

import Wrappers.Map (fromList)

import Test.QuickCheck
import Text.Show

import Game.Vector
import Util.Impure
import Util.Unit

data Units = Size | Time
    deriving (Show, Eq, Enum, Bounded, Ord)

instance Arbitrary Units where
    arbitrary = elements [minBound..maxBound]

-- | Value in the Physics world
type PhysicsValue = Unit Units Double
-- | Unitless value
type Scalar = PhysicsValue
-- | Frictional coefficient
type Mu = Scalar
-- | Against which rates are measured
type Time = PhysicsValue
-- | Measure of space
type Distance = PhysicsValue
-- | Rate of space
type Speed = PhysicsValue
-- | Rate of speed
type Acceleration = PhysicsValue

-- | Dimensions of an object
type Size = Vector Distance
-- | Location in space
type Position = Vector Distance
-- | Change in spatial location
type Velocity = Vector Speed

-- | Collection of physical properties for an object
data Physics = Physics
        { size  :: Size
        , posn  :: Position
        , vcty  :: Velocity
        , accl  :: Vector Acceleration
        , mu    :: Mu
        }
    deriving (Show)

instance Arbitrary Physics where
    arbitrary = Physics <$> (abs <$> arbitrary) <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

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

-- | Create a value with basic units
time, dist :: a -> Unit Units a
-- | Create a unit with derived units
speed, accel :: Fractional a => a -> Unit Units a
time t = t `unit` Time
dist d = d `unit` Size
speed v = dist v / time 1
accel a = speed a / time 1

-- | Get a numeric value from one with units
fromTime, fromDist, fromSpeed, fromAccel :: Num a => Unit Units a -> a
fromTime = fromMaybe (error "fromTime with non-time") . strip (fromList [(Time, 1)])
fromDist = fromMaybe (error "fromDist with non-distance") . strip (fromList [(Size, 1)])
fromSpeed = fromDist . (time 1 *)
fromAccel = fromSpeed . (time 1 *)
