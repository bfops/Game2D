module Game.Physics ( Size
                    , Position
                    , Velocity
                    , Acceleration
                    , Propulsion (..)
                    , TimedPropulsion
                    , Physics (..)
                    , size'
                    , posn'
                    , vcty'
                    , propels'
                    , accl
                    , gravity
                    , bump
                    , overlaps
                    , updatePhysics
                    ) where

import Prelude ()
import Util.Prelewd

import Data.Tuple

import Types

type Size = Vector Double
type Position = Vector Double
type Velocity = Vector Double
type Acceleration = Vector Double
data Propulsion t = Propel Acceleration t

type TimedPropulsion = Propulsion (Maybe DeltaT)

instance Functor Propulsion where
    fmap f (Propel a m) = Propel a (f m)

data Physics = Physics
        { size      :: Size
        , posn      :: Position
        , vcty      :: Velocity
        , propels   :: [TimedPropulsion]
        }

size' :: (Size -> Size) -> Physics -> Physics
size' f p = p { size = f (size p) }

posn' :: (Position -> Position) -> Physics -> Physics
posn' f p = p { posn = f (posn p) }

vcty' :: (Velocity -> Velocity) -> Physics -> Physics
vcty' f p = p { vcty = f (vcty p) }

propels' :: ([TimedPropulsion] -> [TimedPropulsion]) -> Physics -> Physics
propels' f p = p { propels = f (propels p) }

accl :: Physics -> Acceleration
accl p = foldr (liftA2 (+)) (pure 0) $ withoutT <$> propels p
    where
        withoutT (Propel a _) = a

gravity :: TimedPropulsion
gravity = Propel (Vector 0 (-9.81)) Nothing

-- | Index of the smallest element
smallestIndex :: (Num a, Ord a) => Vector a -> Integer
smallestIndex v = fst $ minimumBy (compare `on` abs.snd) $ (,) <$> vector [0..] <*> v

-- | Minimum parameter, if equal return the first
minBy :: Ord a => (a -> a -> Ordering) -> a -> a -> a
minBy f x y = if f x y == GT
              then y
              else x

bump :: DeltaT     -- ^ Time duration of bump
     -> Position   -- ^ Original position of next parameter
     -> Physics    -- ^ Object with which to bump
     -> Position   -- ^ Original position of next parameter
     -> Physics    -- ^ Object to bump
     -> Physics
bump t r2 obj2 r1 obj1 = obj1
        { posn = posn obj1 <&> (+) <*> bumpVector
        -- Velocity changed, since we shifted position
        , vcty = vcty obj1 <&> (+) <*> fmap (/t) bumpVector
        }
    where
        bumpVector = singleV bumpDim $ overlap!bumpDim

        bumpDim = smallestIndex bumpFactors
        bumpFactors = overlap <&> (/) <*> moveVector
        moveVector = (r1 <&> (-) <*> posn obj1) <&> (+) <*> (r2 <&> (-) <*> posn obj2)

        -- Shortest overlap vector (i.e. from the corner of one rectangle to the opposite corner of another)
        overlap = overlap1 <$> posn obj1 <*> size obj1 <*> posn obj2 <*> size obj2
        -- The shortest overlap in 1D
        overlap1 x1 w1 x2 w2 = preferShorter ((x2 + w2) - x1) (x2 - (x1 + w1))
        preferShorter = minBy (compare `on` abs)

overlaps :: Physics -> Physics -> Bool
overlaps p1 p2 = and $ collision1 <$> posn p1 <*> size p1 <*> posn p2 <*> size p2
    where
        -- 1D collision info
        collision1 :: Double -> Double -> Double -> Double -> Bool
        collision1 x1 w1 x2 w2 =  x1 + w1 >= x2
                               && x2 + w2 >= x1

-- | One advancement of physics
updatePhysics :: DeltaT -> Physics -> Physics
updatePhysics t = updatePropels . updateVcty . updatePosn
    where
        updatePosn p = p { posn = posn p <&> (+) <*> fmap (*t) (vcty p) }
        updateVcty p = p { vcty = vcty p <&> (+) <*> fmap (*t) (accl p) }
        updatePropels p = p { propels = mapMaybe stepPropel $ propels p }

        stepPropel :: TimedPropulsion -> Maybe TimedPropulsion
        stepPropel (Propel a (Just t0)) = Propel a . Just <$> decTime t0
        stepPropel p = Just p

        -- Decrement the time by `t`
        -- Returns Nothing on nonpositive values
        decTime :: DeltaT -> Maybe DeltaT
        decTime t0 = mcast (> 0) (t0 - t)
