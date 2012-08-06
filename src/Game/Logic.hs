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
import Util.Range

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

-- Infinite lists of available IDs don't play nicely with deriving Show
instance Show GameState where
    show g = "GameState {objects = " ++ show (objects g) ++ "}"

objects' :: (ObjectGroup -> ObjectGroup) -> GameState -> GameState
objects' f g = g { objects = f (objects g) }

object :: GameState -> ID -> Maybe GameObject
object g i = val <$> find (id <&> (== i)) (objects g)

-- | Cardinal directions
data Direction = Up | Down | Left | Right
    deriving (Eq, Show)

-- | Input events understood by the game
data Input = Jump
           | Move Direction
    deriving (Eq, Show)

addObject :: GameObject -> GameState -> GameState
addObject obj (GameState { ids = (i:is), objects =                  objs })
             = GameState { ids =    is , objects = KeyPair i obj : objs }
addObject _ (GameState { ids = [] }) = error "Ran out of IDs"

deleteObj :: ID -> GameState -> Maybe GameState
deleteObj i (GameState { ids = is, objects = objs })
           = (\os -> GameState { ids = i:is, objects = os }) <$> deleteBy (id <&> (==i)) objs

-- | Start state of the game world
initState :: GameState
initState = let emptyState = GameState { ids = [0..], objects = [] }
            in foldr addObject emptyState [ Platform $ Physics (Vector 4 1) (Vector (-3) (-1)) []
                                          , Player $ Physics (Vector 1 2) (Vector (-3) 0) [gravity $ Vector 1 8]
                                          ]

collide :: GameObject       -- ^ GameObject to update
        -> GameObject       -- ^ GameObject it collided with
        -> GameObject       -- ^ Updated object
collide = const

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
        chopShift = maybe 1 confineTime . start . foldr (<>) (range (Numb 0) (Numb 1))

        confineTime Infinite = 1
        confineTime (Numb x) = x

        collideShift1 shift x1 w1 x2 w2 = pass1 shift (x1 + w1) x2 <> pass1 (negate shift) (x2 + w2) x1

        pass1 v x0 x = let t = (x - x0) / v
                       in case compare v 0 of
                            LT -> range Infinite (Numb t)
                            EQ -> iff (x0 <= x) mempty $ range Infinite Infinite
                            GT -> range (Numb t) Infinite

mult :: Fractional a => Fraction a -> a -> a
mult (Frac n d) x = (n * x) / d

move :: Position                    -- The movement to make (i.e. delta P)
     -> GameObject                  -- Object to move
     -> ObjectGroup                 -- Rest of the objects
     -> (GameObject, ObjectGroup)   -- The object moved to where appropriate, and the necessary collision handlers invoked
move deltaP x = unify . foldr bumpList (Infinite, [], [])
    where
        unify (Infinite, _, _) = error "Something went infinitely wrong"
        unify (Numb t, cs, others) = mapAccumR mutualCollision (phys' (posn' $ makeMove t) x) cs <&> (++ others)

        mutualCollision o1 uo2 = (o1 `collide` val uo2, (`collide` o1) `val'` uo2)

        makeMove t p = let shift = mult t <$> deltaP
                       in p <&> (+) <*> shift

        bumpList iobj acc = keepEarlyColisns iobj acc $ Numb $ collideShift deltaP x $ val iobj

        keepEarlyColisns obj (c1, collides, others) c2 = case compare c1 c2 of
                LT -> (c1, collides, obj:others)
                EQ -> (c1, obj:collides, others)
                GT -> (c2, [obj], collides ++ others)

updateInputs :: [Input] -> GameState -> GameState
updateInputs is = objects' updateObjInputs
    where
        updateObjInputs objs = foldr updateInputAll objs is
        updateInputAll i objs = val' (updateF i) <$> objs

        -- The object update function for a given input
        updateF :: Input -> GameObject -> GameObject
        updateF Jump = ifPlayer $ addDynVcty 0.2 $ Vector 0 8
        updateF (Move d) = ifPlayer $ addDynVcty 0.2 $ moveVcty d

        ifPlayer f obj = if' (isPlayer obj) f obj

        moveVcty Right = Vector 8 0
        moveVcty Left = negate <$> moveVcty Right
        moveVcty _ = pure 0

        addDynVcty :: Time -> Velocity -> GameObject -> GameObject
        addDynVcty t v = phys' $ dvctys' (timedVcty t v :)

-- | One advancement of physics
updateObjPhysics :: Time -> ObjectGroup -> GameObject -> (GameObject, ObjectGroup)
updateObjPhysics t objs = updatePosn . phys' (dvctys' $ mapMaybe $ nextV t)
    where
        updatePosn obj = foldr makeMove (obj, objs) $ vcty <$> dvctys (phys obj)
        makeMove v = uncurry $ move $ (*t) <$> v

updatePhysics :: Time -> GameState -> GameState
updatePhysics t = foldr tryUpdate <*> fmap id . objects
    where
        tryUpdate i g = fromMaybe g $ objPhysWrapper <$> object g i <*> deleteObj i g

        objPhysWrapper obj g = patch g $ updateObjPhysics t (objects g) obj

        patch :: GameState -> (GameObject, ObjectGroup) -> GameState
        patch g (obj, objs) = addObject obj $ g { objects = objs }

update :: [Input] -> Time -> GameState -> GameState
update is t g = foldr ($) g
                    [ updateInputs is
                    , updatePhysics t
                    ]
