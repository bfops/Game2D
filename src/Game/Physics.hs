module Game.Physics ( Size
                    , Position
                    , Velocity
                    , DynamicVelocity
                    , Coord
                    , Time
                    , Physics (..)
                    , vcty
                    , vcty'
                    , size'
                    , posn'
                    , dvctys'
                    , nextV
                    , gravity
                    , timedVcty
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
data DynamicVelocity = DynVcty Velocity (Time -> Maybe DynamicVelocity)

instance Show DynamicVelocity where
    show (DynVcty v _) = "DynVcty (" ++ show v ++ ")"

data Physics = Physics
        { size      :: Size
        , posn      :: Position
        , dvctys    :: [DynamicVelocity]
        }
    deriving (Show)

vcty :: DynamicVelocity -> Velocity
vcty (DynVcty v _) = v

vcty' :: (Velocity -> Velocity) -> DynamicVelocity -> DynamicVelocity
vcty' f (DynVcty v u) = DynVcty (f v) u

size' :: (Size -> Size) -> Physics -> Physics
size' f p = p { size = f (size p) }

posn' :: (Position -> Position) -> Physics -> Physics
posn' f p = p { posn = f (posn p) }

dvctys' :: ([DynamicVelocity] -> [DynamicVelocity]) -> Physics -> Physics
dvctys' f p = p { dvctys = f (dvctys p) }

nextV :: Time -> DynamicVelocity -> Maybe DynamicVelocity
nextV t (DynVcty _ fv) = fv t

gravity :: Velocity -> DynamicVelocity
gravity = (`gravity'` 0)
    where
        gravity' v t = let dv = Vector 0 (-9.81) <&> (*t)
                           v' = v <&> (+) <*> dv
                       in DynVcty v' $ Just . gravity' v'

timedVcty :: Time -> Velocity -> DynamicVelocity
timedVcty t' = DynVcty <*> timedVcty' t'
    where
        timedVcty' t v dt = mcond (dt < t) $ DynVcty v $ timedVcty' (t - dt) v
