{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude
           , FlexibleInstances
           , MultiParamTypeClasses
           #-}
-- | Functions and data structures for dealing with physical game aspects
module Physics.Types ( PhysicsValue
                     , Scalar
                     , Mu
                     , Time
                     , Distance
                     , Speed
                     , Acceleration
                     , Mass
                     , Momentum
                     , Size
                     , Position
                     , Velocity
                     ) where

import Summit.Num.Nonfinite
import Summit.Prelewd
import Summit.Subset.Num

import Data.Fixed
import Text.Show

import Game.Vector
import Util.Unit

data ScalarInternal = Scalar deriving (Show, Eq, Enum, Bounded, Ord)
data DistInternal = Dist deriving (Show, Eq, Enum, Bounded, Ord)
data TimeInternal = Time deriving (Show, Eq, Enum, Bounded, Ord)
data SpeedInternal = Speed deriving (Show, Eq, Enum, Bounded, Ord)
data AccelInternal = Accel deriving (Show, Eq, Enum, Bounded, Ord)
data MassInternal = Mass deriving (Show, Eq, Enum, Bounded, Ord)
data PInternal = MassV deriving (Show, Eq, Enum, Bounded, Ord)

instance UnitMult ScalarInternal a a
instance UnitMult TimeInternal SpeedInternal DistInternal
instance UnitMult TimeInternal AccelInternal SpeedInternal
instance UnitMult SpeedInternal MassInternal PInternal

-- | Root value type
type PhysicsValue = Milli

-- | Unitless value
type Scalar = Unit ScalarInternal PhysicsValue

-- | Frictional coefficient
type Mu = Scalar
-- | Against which rates are measured
type Time = Nonnegative (Unit TimeInternal PhysicsValue)
-- | Measure of space
type Distance = Unit DistInternal PhysicsValue
-- | Rate of space
type Speed = Unit SpeedInternal PhysicsValue
-- | Rate of speed
type Acceleration = Unit AccelInternal PhysicsValue
-- | Resistance to acceleration
type Mass = Nonfinite (Positive (Unit MassInternal PhysicsValue))
type Momentum = Unit PInternal PhysicsValue

-- | Dimensions of an object
type Size = Vector (Positive Distance)
-- | Location in space
type Position = Vector Distance
-- | Change in spatial location
type Velocity = Vector Speed
