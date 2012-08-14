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

import Data.Fixed

import Types

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
