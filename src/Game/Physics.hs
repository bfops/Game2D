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
                    , gravity
                    , bump
                    , overlaps
                    , updatePhysics
                    ) where

import Prelude ()
import Util.Prelewd

import Data.Fixed hiding (div')
import Data.Tuple
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

deltaV :: Physics -> Time -> Velocity
deltaV p t = sum <$> sequenceA deltaVs
    where
        deltaVs = deltaV' <$> propels p
        deltaV' (Propel a pt) = a <&> (* chopT pt)
        -- The amount of time an acceleration applies for
        chopT pt = fromMaybe t $ min t <$> pt

gravity :: TimedPropulsion
gravity = Propel (Vector 0 (-9.81)) Nothing

-- | Index of the smallest Just element (by magnitude)
minExtantIndex :: (Num a, Ord a) => Vector (Maybe a) -> Integer
minExtantIndex v = fst $ minimumBy (compare `on` weight) $ (,) <$> vector [0..] <*> v
    where
        weight (_, mw) = abs <$> mw

-- | Division, but returns Nothing if the second parameter is 0
div' :: (Eq a, Fractional a) => a -> a -> Maybe a
div' x y = (x/) <$> mcast (/= 0) y

bump :: Position   -- ^ Original position of next parameter
     -> Physics    -- ^ Object with which to bump
     -> Position   -- ^ Original position of next parameter
     -> Physics    -- ^ Object to bump
     -> Physics
bump r2 obj2 r1 obj1 = obj1
        { posn = posn obj1 <&> (+) <*> bumpVector
        -- Set velocity to that of the object we're pushed by
        , vcty = setV bumpDim (vcty obj2!bumpDim) $ vcty obj1
        }
    where
        bumpVector = singleV bumpDim $ bumps!bumpDim

        -- Which dimension to bump in
        bumpDim = minExtantIndex timeBumps
        -- Vector of times denoting how long the objects have been overlapping in each dimension
        timeBumps = bumps <&> div' <*> relativeDisp
        -- Relative displacement between the objects, since the beginning of the frame
        relativeDisp = (posn obj2 <&> (-) <*> r2) <&> (-) <*> (posn obj1 <&> (-) <*> r1)

        -- Vector of minimum bumps in each dimension
        bumps = overlap <$> posn obj1 <*> size obj1 <*> posn obj2 <*> size obj2 <*> relativeDisp
        -- The 1D overlap between two lines
        overlap x1 w1 x2 w2 d = if d > 0
                                then x2 + w2 - x1
                                else x1 + w1 - x2

overlaps :: Physics -> Physics -> Bool
overlaps p1 p2 = and $ collision1 <$> posn p1 <*> size p1 <*> posn p2 <*> size p2
    where
        -- 1D collision info
        collision1 :: Coord -> Coord -> Coord -> Coord -> Bool
        collision1 x1 w1 x2 w2 =  x1 + w1 > x2
                               && x2 + w2 > x1

-- | One advancement of physics
updatePhysics :: Time -> Physics -> Physics
updatePhysics t = updatePropels . updateVcty . updatePosn
    where
        updatePosn p = p { posn = posn p <&> (+) <*> fmap (*t) (vcty p) }
        updateVcty p = p { vcty = vcty p <&> (+) <*> deltaV p t }
        updatePropels p = p { propels = mapMaybe stepPropel $ propels p }

        stepPropel :: TimedPropulsion -> Maybe TimedPropulsion
        stepPropel (Propel a (Just t0)) = Propel a . Just <$> decTime t0
        stepPropel p = Just p

        -- Decrement the time by `t`
        -- Returns Nothing on nonpositive values
        decTime :: Time -> Maybe Time
        decTime t0 = mcast (> 0) (t0 - t)
