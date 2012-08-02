module Game.Physics ( Size
                    , Position
                    , Velocity
                    , Acceleration
                    , Coord
                    , Time
                    , Propulsion (..)
                    , TimedPropulsion
                    , Physics (..)
                    , size'
                    , posn'
                    , vcty'
                    , propels'
                    , space
                    , gravity
                    , overlaps
                    ) where

import Prelude ()
import Util.Prelewd

import Data.Fixed
import Text.Show

import Types

type Coord = Milli
type Time = Coord

type Size = Vector Coord
type Position = Vector Coord
type Velocity = Vector Coord
type Acceleration = Vector Coord
data Propulsion t = Propel Acceleration t
    deriving (Show)

instance Eq a => Eq (Propulsion a) where
    (Propel a t) == (Propel a' t') = a == a' && t == t'

type TimedPropulsion = Propulsion (Maybe Time)

instance Functor Propulsion where
    fmap f (Propel a m) = Propel a (f m)

data Physics = Physics
        { size      :: Size
        , posn      :: Position
        , vcty      :: Velocity
        , propels   :: [TimedPropulsion]
        }
    deriving (Show)

size' :: (Size -> Size) -> Physics -> Physics
size' f p = p { size = f (size p) }

posn' :: (Position -> Position) -> Physics -> Physics
posn' f p = p { posn = f (posn p) }

vcty' :: (Velocity -> Velocity) -> Physics -> Physics
vcty' f p = p { vcty = f (vcty p) }

propels' :: ([TimedPropulsion] -> [TimedPropulsion]) -> Physics -> Physics
propels' f p = p { propels = f (propels p) }

-- | Get the space occupied by an object
space :: Physics -> (Position, Size)
space = posn <&> (,) <*> size

gravity :: TimedPropulsion
gravity = Propel (Vector 0 (-9.81)) Nothing

overlaps :: (Position, Size) -> (Position, Size) -> Bool
overlaps (p1, s1) (p2, s2) = and $ collision1 <$> p1 <*> s1 <*> p2 <*> s2
    where
        -- 1D collision info
        collision1 :: Coord -> Coord -> Coord -> Coord -> Bool
        collision1 x1 w1 x2 w2 =  x1 + w1 > x2
                               && x2 + w2 > x1
