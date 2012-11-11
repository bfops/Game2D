-- | Functions and data structures for dealing with physical game aspects
module Game.Physics ( PhysicsValue
                    , Scalar
                    , Time
                    , Distance
                    , Speed
                    , Acceleration
                    , Mass
                    , Size
                    , Position
                    , Velocity
                    , Physics (..)
                    , size'
                    , mass'
                    , posn'
                    , vcty'
                    , accl'
                    , mu'
                    ) where

import Prelewd

import Data.Fixed
import Num.Indeterminate
import Template.MemberTransformer
import Test.QuickCheck
import Text.Show

import Game.Vector
import Util.Unit

data ScalarInternal = Scalar deriving (Show, Eq, Enum, Bounded, Ord)
data DistInternal = Dist deriving (Show, Eq, Enum, Bounded, Ord)
data TimeInternal = Time deriving (Show, Eq, Enum, Bounded, Ord)
data SpeedInternal = Speed deriving (Show, Eq, Enum, Bounded, Ord)
data AccelInternal = Accel deriving (Show, Eq, Enum, Bounded, Ord)
data MassInternal = Mass deriving (Show, Eq, Enum, Bounded, Ord)

instance UnitMult ScalarInternal a a
instance UnitMult TimeInternal SpeedInternal DistInternal
instance UnitMult TimeInternal AccelInternal SpeedInternal

-- | Root value type
type PhysicsValue = Milli

-- | Unitless value
type Scalar = Unit ScalarInternal PhysicsValue

-- | Frictional coefficient
type Mu = Scalar
-- | Against which rates are measured
type Time = Unit TimeInternal PhysicsValue
-- | Measure of space
type Distance = Unit DistInternal PhysicsValue
-- | Rate of space
type Speed = Unit SpeedInternal PhysicsValue
-- | Rate of speed
type Acceleration = Unit AccelInternal PhysicsValue
-- | Resistance to acceleration
type Mass = Unit MassInternal (Indeterminate PhysicsValue)

-- | Dimensions of an object
type Size = Vector Distance
-- | Location in space
type Position = Vector Distance
-- | Change in spatial location
type Velocity = Vector Speed

-- | Collection of physical properties for an object
data Physics = Physics
        { size :: Size
        , mass :: Mass
        , posn :: Position
        , vcty :: Velocity
        , accl :: Vector Acceleration
        , mu   :: Mu
        }
    deriving (Show, Eq)

$(memberTransformers ''Physics)

instance Arbitrary Physics where
    arbitrary = Physics
              <$> sequence (pure $ abs <$> (arbitrary `suchThat` (/= 0)))
              <*> (abs <$> arbitrary)
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
