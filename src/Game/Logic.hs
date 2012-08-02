module Game.Logic ( GameObject (..)
                  , phys'
                  , isPlatform
                  , isBlock
                  , isPlayer
                  , KeyPair
                  , id
                  , val
                  , val'
                  , GameState (..)
                  , objects'
                  , Input (..)
                  , Direction(..)
                  , initState
                  , update
                  ) where

import Prelude ()
import Util.Prelewd hiding (id)

import Data.Tuple
import Text.Show

import Util.Fraction
import Util.Impure

import Game.Physics

import Types

-- | An object in the game world
data GameObject
        = Block    { phys :: Physics }
        | Platform { phys :: Physics }
        | Player   { phys :: Physics }
    deriving (Show)

phys' :: (Physics -> Physics) -> GameObject -> GameObject
phys' f g = g { phys = f (phys g) }

isBlock :: GameObject -> Bool
isBlock (Block {}) = True
isBlock _ = False

isPlatform :: GameObject -> Bool
isPlatform (Platform {}) = True
isPlatform _ = False

isPlayer :: GameObject -> Bool
isPlayer (Player {}) = True
isPlayer _ = False

data KeyPair a b = KeyPair a b
    deriving (Show)

id :: KeyPair a b -> a
id (KeyPair a _) = a

val :: KeyPair a b -> b
val (KeyPair _ b) = b

val' :: (b -> b) -> KeyPair a b -> KeyPair a b
val' f (KeyPair a b) = KeyPair a (f b)

instance (Eq a) => Eq (KeyPair a b) where
    (==) = (==) `on` id

type ID = Integer
type UniqueObject = KeyPair ID GameObject
type ObjectGroup = [UniqueObject]

data GameState = GameState { objects :: ObjectGroup
                           , ids     :: [ID]
                           }
    deriving (Show)

objects' :: (ObjectGroup -> ObjectGroup) -> GameState -> GameState
objects' f g = g { objects = f (objects g) }

-- | Cardinal directions
data Direction = Up | Down | Left | Right
    deriving (Eq, Show)

-- | Input events understood by the game
data Input = Jump
           | Move Direction
    deriving (Eq, Show)

addObject :: GameObject -> GameState -> GameState
addObject obj (GameState { ids = (i:ids'), objects =                  objs })
             = GameState { ids =    ids' , objects = KeyPair i obj : objs }
addObject _ (GameState { ids = [] }) = error "Ran out of IDs"

-- | Start state of the game world
initState :: GameState
initState = let emptyState = GameState { ids = [0..], objects = [] }
            in foldr addObject emptyState [ Platform $ Physics (Vector 4 1) (Vector (-3) (-1)) (Vector 0 0) []
                                          , Player $ Physics (Vector 1 2) (Vector (-3) 0) (Vector 0 0) [gravity]
                                          ]

collide :: GameObject       -- ^ GameObject to update
                 -> GameObject       -- ^ GameObject it collided with
                 -> GameObject       -- ^ Updated object
collide = const

type Range a = Maybe (a, a)

collideShift :: Position -> GameObject -> GameObject -> Fraction Coord
collideShift d o1 o2 = let
                        shift = realToFrac <$> d
                        p1 = realToFrac <$> posn (phys o1)
                        s1 = realToFrac <$> size (phys o1)
                        p2 = realToFrac <$> posn (phys o2)
                        s2 = realToFrac <$> size (phys o2)
                       in chopShift $ collideShift1 <$> shift <*> p1 <*> s1 <*> p2 <*> s2
    where
        chopShift :: Vector (Range (Fraction Coord)) -> Fraction Coord
        chopShift = maybe 1 fst . foldr1 intrsctRange

        intrsctRange mr1 mr2 = do
                                (t1, t2) <- mr1
                                (q1, q2) <- mr2
                                iff (t1 > t2 || q1 > q2)
                                    (error "Invalid range")
                                    (range (max t1 q1) (min t2 q2))

        -- Construct a range, if it's valid
        range :: Ord a => a -> a -> Range a
        range x y = mcond (x <= y) (x, y)

        collideShift1 shift x1 w1 x2 w2 = intrsctRange (pointShift shift x1 $ x2 + w2)
                                                       (pointShift shift x2 $ x1 + w1)

        pointShift v x0 x = let t = (x - x0) / v
                            in if x0 < x
                               then mcond (t >= 0) (0, t)
                               else Just (max t 0, 1/0)

mult :: Fractional a => Fraction a -> a -> a
mult (Frac n d) x = (n * x) / d

move :: Position                    -- The movement to make (i.e. delta P)
     -> GameObject                  -- Object to move
     -> ObjectGroup                 -- Rest of the objects
     -> (GameObject, ObjectGroup)   -- The object moved to where appropriate, and the necessary collision handlers invoked
move p x = unify . foldr bumpList (1/0, [], [])
    where
        unify (t, cs, others) = mapAccumR mutualCollision (phys' (posn' $ makeMove t) x) cs <&> (++ others)

        mutualCollision o1 uo2 = (o1 `collide` val uo2, (`collide` o1) `val'` uo2)

        makeMove t px = let dp = mult t <$> (p <&> (-) <*> px)
                        in traceShow dp $ px <&> (+) <*> dp

        bumpList iobj acc = keepEarlyColisns iobj acc $ collideShift p x $ val iobj

        keepEarlyColisns obj (c1, collides, others) c2 = case compare c1 c2 of
                LT -> (c1, collides, obj:others)
                GT -> (c2, [obj], collides ++ others)
                EQ -> (c2, obj:collides, others)

updateInputs :: [Input] -> GameState -> GameState
updateInputs is = objects' updateObjInputs
    where
        updateObjInputs objs = foldr updateInputAll objs is
        updateInputAll i objs = let updateFunc = updateF i
                                in (updateFunc `val'`) <$> objs

        -- The object update function for a given input
        updateF Jump = jumpIfPlayer
        updateF (Move d) = moveIfPlayer d

        jumpIfPlayer :: GameObject -> GameObject
        jumpIfPlayer obj = if' (isPlayer obj) (phys' $ propels' (jump ++)) obj

        moveIfPlayer :: Direction -> GameObject -> GameObject
        moveIfPlayer d obj = if' (isPlayer obj) (phys' $ propels' (moveVector d ++)) obj

        jump = [Propel (Vector 0 80) (Just 0.1)]

        moveVector Left  = [Propel (Vector (-40) 0) (Just 0.1)]
        moveVector Right = [Propel (Vector   40  0) (Just 0.1)]
        moveVector _ = []

-- | One advancement of physics
updateObjPhysics :: Time -> ObjectGroup -> GameObject -> (GameObject, ObjectGroup)
updateObjPhysics t objs = updatePosn . phys' updateVcty . phys' updatePropels
    where
        updatePosn obj = move ((*t) <$> vcty (phys obj)) obj $ objs
        updateVcty p = p { vcty = vcty p <&> (+) <*> deltaV p t }
        updatePropels = propels' $ mapMaybe stepPropel

        stepPropel :: TimedPropulsion -> Maybe TimedPropulsion
        stepPropel (Propel a (Just t0)) = Propel a . Just <$> decTime t0
        stepPropel p = Just p

        -- Decrement the time by `t`
        -- Returns Nothing on nonpositive values
        decTime :: Time -> Maybe Time
        decTime t0 = mcast (> 0) (t0 - t)

deltaV :: Physics -> Time -> Velocity
deltaV p t = sum <$> sequenceA deltaVs
    where
        deltaVs = deltaV' <$> propels p
        deltaV' (Propel a pt) = a <&> (* chopT pt)
        -- The amount of time an acceleration applies for
        chopT pt = fromMaybe t $ min t <$> pt

extract :: Eq a => (v a b -> a) -> (v a b -> b) -> a -> [v a b] -> Maybe (b, [v a b])
extract _ _ _ [] = Nothing
extract a b x (y : ys) = if x == a y
                         then Just (b y, ys)
                         else (y:) $$ extract a b x ys

updatePhysByID :: Time -> ID -> ObjectGroup -> Maybe (GameObject, ObjectGroup)
updatePhysByID t i objs = uncurry (flip $ updateObjPhysics t) <$> extract id val i objs

updatePhysics :: Time -> GameState -> GameState
updatePhysics t = foldr tryUpdate <*> fmap id . objects
    where
        tryUpdate i g = maybe g (patch g) $ updatePhysByID t i $ objects g

        patch :: GameState -> (GameObject, ObjectGroup) -> GameState
        patch g (obj, objs) = addObject obj $ g { objects = objs }

update :: [Input] -> Time -> GameState -> GameState
update is t g = foldr ($) g
                    [ updateInputs is
                    , updatePhysics t
                    ]
