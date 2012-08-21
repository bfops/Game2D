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
                    ) where

import Prelude ()
import Util.Prelewd

import Data.Fixed
import Text.Show

import Util.Impure
import Game.Vector

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

gravity :: Acceleration
gravity = Vector 0 (-32)
