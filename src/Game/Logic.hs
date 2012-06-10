module Game.Logic ( GameObject (..)
                  , ObjectType (..)
                  , GameState (..)
                  , Input (..)
                  , initState
                  , update
                  ) where

import Prelude ()
import Util.Prelewd

import Data.Tuple

import Util.Types

type Position = Vector Double
type Size = Vector Double
type Velocity = Vector Double

data ObjectType = Block
                | Platform
    deriving (Eq)

data GameObject = GameObject { objType :: ObjectType
                             , posn :: Position
                             , vcty :: Velocity
                             , size :: Size
                             }

data GameState = GameState { objects :: [GameObject]
                           }

data Input = MoveUp
    deriving (Eq)

-- | Fold right-to-left and generate a list.
foldCons :: (b -> a -> (b, a))
         -> b
         -> [a]
         -> (b, [a])
foldCons f x0 = foldr (\x (b, l) -> (:l) <$> f b x) (x0, []) 

-- | Start state of the game world
initState :: GameState
initState = GameState [ GameObject Block (Vector 0 0) (Vector 1 10) (Vector 1 1)
                      , GameObject Platform (Vector 0 (-1.5)) (Vector 0 0) (Vector 4 1)
                      ]

collisionHandler :: GameObject       -- ^ Object to update
                 -> GameObject       -- ^ Object it collided with
                 -> Vector Collision -- ^ Description of the specific collision
                 -> GameObject       -- ^ Updated object
collisionHandler g1 g2 vc
        | objType g2 == Platform
            = bump fixV g2
        | otherwise
            = g1
    where
        fixV = if snd (vc!1)
               then g1 { vcty = Vector (vcty g1!0) 0 }
               else g1

-- | `bump p q` returns `p`, readjusted to not overlap `q`.
bump :: GameObject -> GameObject -> GameObject
bump p1 p2 = p1 { posn = p' }
    where
        p' = if magLess bx by
             then Vector (x - bx) y
             else Vector x (y - by)
        Vector x y = posn p1
        (Vector bx by) = (\a b -> iff (magLess a b) a b) <$> lowDiff <*> highDiff
        pDiff = (-) <$> posn p1 <*> posn p2
        lowDiff = (-) <$> pDiff <*> size p2
        highDiff = (+) <$> pDiff <*> size p1

type Collision = (Bool, Bool) -- ^ Overlap on low and high object borders, respectively.

collision :: GameObject -> GameObject -> Maybe (Vector Collision)
collision g1 g2 = sequenceA $ collision1 <$> posn g1 <*> size g1 <*> posn g2 <*> size g2
    where
        collision1 x1 w1 x2 w2 = (x1 <= x2, x1 + w1 >= x2 + w2) `mcond` colliding x1 w1 x2 w2
        colliding x1 w1 x2 w2 =  x1 + w1 >= x2
                              && x2 + w2 >= x1

tryCollide :: GameObject -- ^ Object to update
           -> GameObject -- ^ Object it collided with
           -> GameObject -- ^ Updated object
tryCollide g1 g2 = maybe g1 (collisionHandler g1 g2) $ collision g1 g2

-- | Like (<), but compares on magnitude
magLess :: (Num a, Ord a) => a -> a -> Bool
magLess = (<) `on` abs

updatePhysics :: [Input] -> DeltaT -> GameObject -> GameObject
updatePhysics inputs t = if' ((`any` inputs) (== MoveUp))
                             (updateMove .)
                             (updateVcty . updatePosn)
    where
        updatePosn g = g { posn = (+) <$> posn g <*> fmap (*t) (vcty g) }
        updateMove g = if objType g == Platform
                        then g { posn = (+) <$> posn g <*> Vector 0 0.3 }
                        else g
        updateVcty g
            | objType g == Block    = g { vcty = (+) <$> vcty g <*> fmap (*t) a_g }
            | otherwise             = g
        a_g = Vector 0 (-9.81)

update :: GameState -> [Input] -> DeltaT -> GameState
update gs is t = gs { objects = foldr (bumpCons . updatePhysics is t) [] $ objects gs }
    where
        -- | Cons the object on to the list, as well as bump every other object with it.
        bumpCons obj = (\(x, l) -> x : l) . foldCons collide2 obj
        collide2 o1 o2 = (tryCollide o1 o2, tryCollide o2 o1)
