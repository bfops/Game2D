module Game.Logic ( GameObject (..)
                  , GameState (..)
                  , Input (..)
                  , Direction(..)
                  , initState
                  , update
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

type TimedPropel = Propulsion (Maybe DeltaT)

instance Functor Propulsion where
    fmap f (Propel a m) = Propel a (f m)

-- | An object in the game world
data GameObject
    = Block { size     :: Size
            , posn     :: Position
            , vcty     :: Velocity
            , propels  :: [TimedPropel]
            }
    | Platform { size       :: Size
               , posn       :: Position
               , vcty       :: Velocity
               , propels    :: [TimedPropel]
               }

accl :: GameObject -> Acceleration
accl g = foldr (liftA2 (+)) (pure 0) $ withoutT <$> propels g
    where
        withoutT (Propel a _) = a

isBlock :: GameObject -> Bool
isBlock (Block {}) = True
isBlock _ = False

isPlatform :: GameObject -> Bool
isPlatform (Platform {}) = True
isPlatform _ = False

data GameState = GameState { objects :: [GameObject]
                           }

-- | Cardinal directions
data Direction = Up | Down | Left | Right
    deriving (Eq)

-- | Input events understood by the game
data Input = Move Direction
    deriving (Eq)

-- | Fold right-to-left and generate a list
foldCons :: (b -> a -> (b, a))
         -> b
         -> [a]
         -> (b, [a])
foldCons f x0 = foldr (\x (b, l) -> (:l) <$> f b x) (x0, []) 

gravity :: TimedPropel
gravity = Propel (Vector 0 (-9.81)) Nothing

-- | Start state of the game world
initState :: GameState
initState = GameState [ Block (Vector 1 1) (Vector 0 0) (Vector 1 10) [gravity]
                      , Platform (Vector 4 1) (Vector (-3) (-1)) (Vector 2 0) []
                      , Platform (Vector 1 4) (Vector (-8) (-4)) (Vector 0 0) [Propel (Vector 1 0) (Just 3)]
                      ]

collisionHandler :: DeltaT           -- ^ Time interval of collision
                 -> Position         -- ^ Original position of first object
                 -> Position         -- ^ Original position of second object
                 -> GameObject       -- ^ Object to update
                 -> GameObject       -- ^ Object it collided with
                 -> GameObject       -- ^ Updated object
collisionHandler t p1 p2 g1 g2 = if' (isBlock g1 || isPlatform g2) (bump t p2 g2 p1) g1

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
     -> GameObject -- ^ Object with which to bump
     -> Position   -- ^ Original position of next parameter
     -> GameObject -- ^ Object to bump
     -> GameObject
bump t r2 g2 r1 g1 = g1
        { posn = posn g1 <&> (+) <*> bumpVector
        -- Velocity changed, since we shifted position
        , vcty = vcty g1 <&> (+) <*> fmap (/t) bumpVector
        }
    where
        bumpVector = singleV bumpDim $ overlap!bumpDim

        bumpDim = smallestIndex bumpFactors
        bumpFactors = overlap <&> (/) <*> moveVector
        moveVector = (r1 <&> (-) <*> posn g1) <&> (+) <*> (r2 <&> (-) <*> posn g2)

        -- Shortest overlap vector (i.e. from the corner of one rectangle to the opposite corner of another)
        overlap = overlap1 <$> posn g1 <*> size g1 <*> posn g2 <*> size g2
        -- The shortest overlap in 1D
        overlap1 x1 w1 x2 w2 = preferShorter ((x2 + w2) - x1) (x2 - (x1 + w1))
        preferShorter = minBy (compare `on` abs)

collision :: GameObject -> GameObject -> Bool
collision g1 g2 = and $ collision1 <$> posn g1 <*> size g1 <*> posn g2 <*> size g2
    where
        -- 1D collision info
        collision1 :: Double -> Double -> Double -> Double -> Bool
        collision1 x1 w1 x2 w2 =  x1 + w1 >= x2
                               && x2 + w2 >= x1

-- | If the objects collide, call the appropriate handlers; otherwise just return
tryCollide :: DeltaT     -- ^ Time interval of collision
           -> Position   -- ^ Original position of the object to bump
           -> Position   -- ^ Original position of the object it collided with
           -> GameObject -- ^ Object to bump
           -> GameObject -- ^ Object it collided with
           -> GameObject -- ^ Updated object
tryCollide t p1 p2 = bool $$ const $* collisionHandler t p1 p2 $* collision

-- | One advancement of physics
updatePhysics :: DeltaT -> GameObject -> GameObject
updatePhysics t = updatePropels . updateVcty . updatePosn
    where
        updatePosn g = g { posn = posn g <&> (+) <*> fmap (*t) (vcty g) }
        updateVcty g = g { vcty = vcty g <&> (+) <*> fmap (*t) (accl g) }
        updatePropels g = g { propels = mapMaybe stepPropel $ propels g }

        stepPropel :: TimedPropel -> Maybe TimedPropel
        stepPropel (Propel a (Just t0)) = Propel a . Just <$> decTime t0
        stepPropel p = Just p

        -- Decrement the time by `t`
        -- Returns Nothing on nonpositive values
        decTime :: DeltaT -> Maybe DeltaT
        decTime t0 = mcast (> 0) (t0 - t)

updateInputs :: [Input] -> GameState -> GameState
updateInputs is g = g { objects = foldr updateInput (objects g) is }
    where
        -- Update a list of gameobjects with an input
        updateInput :: Input -> [GameObject] -> [GameObject]
        updateInput (Move _) = fmap $ bool <*> id <*> isPlatform
        updateInput _ = id

-- | Ensure no objects are colliding
updateBumps :: DeltaT
            -> [(Position, GameObject)] -- ^ [(originalPos, obj)]
            -> [GameObject]             -- ^ Updated objects
updateBumps t = (snd <$>) . foldr bumpCons []
    where
        -- | Cons the object on to the list, as well as bump every other object with it.
        bumpCons obj = (\(x, l) -> x : l) . foldCons collide2 obj
        collide2 (p1, o1) (p2, o2) = ((p1, tryCollide t p1 p2 o1 o2), (p2, tryCollide t p2 p1 o2 o1))

update :: [Input] -> DeltaT -> GameState -> GameState
update is t g@(GameState { objects = o }) = updateInputs is $ g { objects = bumpedObjects }
    where
        bumpedObjects = updateBumps t $ zip (posn <$> o) $ updatePhysics t <$> o
