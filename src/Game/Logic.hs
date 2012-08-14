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
import Util.Prelewd hiding (id, empty)

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
           = (\os -> GameState { ids = i:is, objects = os }) <$> deleteBy (id <&> (== i)) objs

-- | Start state of the game world
initState :: GameState
initState = let emptyState = GameState { ids = [0..], objects = [] }
            in foldr addObject emptyState [ Platform $ Physics (Vector 4 1) (Vector (-3) (-1)) (Vector 0 0) (Vector 0 0)
                                          , Player $ Physics (Vector 1 2) (Vector (-3) 0) (Vector 0 0) gravity
                                          ]

collide :: GameObject       -- ^ GameObject to update
        -> GameObject       -- ^ GameObject it collided with
        -> GameObject       -- ^ Updated object
collide = const

data TaggedRange a b = TaggedRange a (Range b)
    deriving Show

infToMaybe :: InfNum a -> Maybe a
infToMaybe Infinite = Nothing
infToMaybe (Numb x) = Just x

taggedOverlap :: (Show a, Show b, Ord b, Monoid a) => TaggedRange a b -> TaggedRange a b -> TaggedRange a b
taggedOverlap (TaggedRange x r1) (TaggedRange y r2) = let rng = r1 <> r2
                                                          tag = if rng == empty
                                                                then mempty
                                                                -- This quite rightly causes an error when it fails
                                                                else fromJust $ overlap <$> start r1 <*> start r2
                                                      in TaggedRange tag rng
        where
            overlap s1 s2 = case (compare `on` infToMaybe) s1 s2 of
                                LT -> y
                                EQ -> x <> y
                                GT -> x

collideShift :: Position -> GameObject -> GameObject -> ([Dimension], Fraction Coord)
collideShift deltaP o1 o2 = let
                                shift = realToFrac <$> deltaP
                                p1 = realToFrac <$> posn (phys o1)
                                s1 = realToFrac <$> size (phys o1)
                                p2 = realToFrac <$> posn (phys o2)
                                s2 = realToFrac <$> size (phys o2)
                                -- Collisions individually in each dimension
                                collides1 = collideShift1 <$> dimensions <*> shift <*> p1 <*> s1 <*> p2 <*> s2
                                TaggedRange dims rng = foldr1 taggedOverlap $ normalize <$> collides1
                            in maybe ([], 1) ((dims,) . recast) $ start rng
    where
        recast (Numb x) = x
        recast Infinite = error "recast parameter should be finite"

        -- Chop all time ranges to [0, 1]
        normalize (TaggedRange t r) = let r' = range (Numb 0) (Numb 1) <> r
                                      in if r' == empty
                                         then TaggedRange mempty empty
                                         else TaggedRange t r'

        collideShift1 d shift x1 w1 x2 w2 = TaggedRange [d] $ pass1 shift (x1 + w1) x2 <> pass1 (negate shift) (x2 + w2) x1

        pass1 v x0 x = let t = (x - x0) / v
                       in case compare v 0 of
                        LT -> range Infinite (Numb t)
                        EQ -> iff (x0 <= x) empty $ range Infinite Infinite
                        GT -> range (Numb t) Infinite

upd1 :: (a -> r) -> (a, b, c) -> (r, b, c)
upd1 f (a, b, c) = (f a, b, c)

move :: Position                        -- The movement to make (i.e. delta P)
     -> GameObject                      -- Object to move
     -> ObjectGroup                     -- Rest of the objects
     -> (Position, [ID], [Dimension])   -- The amount the object can be moved, the IDs it collides with, and how it collides
move deltaP x = upd1 resolveT . foldr bumpList (Infinite, [], [])
    where
        resolveT Infinite = error "Something went infinitely wrong"
        resolveT (Numb t) = assert (t >= 0 && t <= 1) $ (realToFrac . (realToFrac t *)) <$> deltaP

        bumpList iobj acc = keepEarlyColisns iobj acc $ collideShift deltaP x $ val iobj

        keepEarlyColisns obj (c1, collides, dims) (ds, k) = let c2 = Numb k
                                                            in case compare c1 c2 of
                                                                LT -> (c1, collides, dims)
                                                                EQ -> (c2, id obj:collides, ds ++ dims)
                                                                GT -> (c2, [id obj], ds)

updateInputs :: [Input] -> GameState -> GameState
updateInputs is = objects' updateObjInputs
    where
        updateObjInputs objs = foldr updateInputAll objs is
        updateInputAll i objs = val' (updateF i) <$> objs

        -- The object update function for a given input
        updateF :: Input -> GameObject -> GameObject
        updateF Jump = ifPlayer $ addVcty $ Vector 0 8
        updateF (Move d) = ifPlayer $ addVcty $ moveVcty d

        ifPlayer f obj = if' (isPlayer obj) f obj

        moveVcty Right = Vector 8 0
        moveVcty Left = negate <$> moveVcty Right
        moveVcty _ = pure 0

        addVcty :: Velocity -> GameObject -> GameObject
        addVcty v = phys' $ vcty' $ liftA2 (+) v

extract :: ID -> ObjectGroup -> Maybe (UniqueObject, ObjectGroup)
extract _ [] = Nothing
extract i (obj:objs) = if i == id obj
                       then Just (obj, objs)
                       else ((obj:) <$>)<$> extract i objs

isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (\d x -> setV d x $ pure zero) dimensions

setV :: Dimension -> a -> Vector a -> Vector a
setV d1 x = liftA2 (\d2 -> iff (d1 == d2) x) dimensions

-- | One advancement of physics
updateObjPhysics :: Time -> ObjectGroup -> GameObject -> (GameObject, ObjectGroup)
updateObjPhysics t others = updatePosn others . phys' updateVcty
    where
        updateVcty p = p { vcty = accl p <&> (*t) <&> (+) <*> vcty p }
        updatePosn objs obj = unify $ mapAccumR moveAndCollide (obj, objs) $ isolate 0 $ vcty $ phys obj

        unify ((obj, objs), vs) = (phys' (vcty' $ const $ foldr (liftA2 (+)) (pure 0) vs) obj, objs)
        moveAndCollide (obj, objs) v = let (deltaP, collides, dims) = move ((*t) <$> v) obj objs
                                       in (enactCollides obj objs deltaP collides, foldr (`setV` 0) v dims)

        enactCollides :: GameObject -> ObjectGroup -> Position -> [ID] -> (GameObject, ObjectGroup)
        enactCollides obj objs deltaP collides = foldr findAndHit (makeMove deltaP obj, objs) collides
        
        findAndHit i (obj, objs) = maybe (error $ "Couldn't find object " ++ show i) (collideCons obj) $ extract i objs
        collideCons o1 (o2, objs) = mutualCollide o1 o2 <&> (:objs)

        makeMove :: Position -> GameObject -> GameObject
        makeMove = phys' . posn' . liftA2 (+)

        mutualCollide o1 o2 = (o1 `collide` val o2, val' (`collide` o1) o2)

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
