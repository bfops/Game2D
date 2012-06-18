module Game.Logic ( GameObject (..)
                  , GameState (..)
                  , Input (..)
                  , Direction(..)
                  , initState
                  , update
                  ) where

import Prelude ()
import Util.Prelewd

import Types

type Size = Vector Double
type Position = Vector Double
type Velocity = Vector Double

-- | An object in the game world
data GameObject
    = Block { size     :: Size
            , posn     :: Position
            , vcty     :: Velocity
            }
    | Platform { size   :: Size
               , posn   :: Position
               , vcty   :: Velocity
               , moveT  :: Maybe DeltaT -- ^ Time spent moving; Nothing if not moving
               }

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

-- | Start state of the game world
initState :: GameState
initState = GameState [ Block (Vector 1 1) (Vector 0 0) (Vector 1 10)
                      , Platform (Vector 4 1) (Vector 0 (-1)) (Vector 0 0) Nothing
                      ]

collisionHandler :: GameObject       -- ^ Object to update
                 -> GameObject       -- ^ Object it collided with
                 -> Vector Collision -- ^ Description of the specific collision
                 -> GameObject       -- ^ Updated object
collisionHandler g1 g2 _ = if' (isBlock g1 || isPlatform g2) (`bump` g2) g1

-- | `bump p q` returns `p`, readjusted in position to not overlap `q`.
bump :: GameObject -> GameObject -> GameObject
bump p1 p2 = let
                allBumps = bumps1 <$> posn p1 <*> size p1 <*> posn p2 <*> size p2
                -- The bumps as vectors
                vBumps = concat $ fmap . singleV <$> vector [0..] <*> allBumps
                bumpVector = shortest $ filter backwardBump vBumps
             in p1 { posn = posn p1 <&> (-) <*> bumpVector
                   -- Velocity in the bumped direction becomes 0
                   , vcty = (\b k -> iff (b == 0) k 0) <$> bumpVector <*> vcty p1
                   }
    where
        relativeV = vcty p1 <&> (-) <*> vcty p2

        shortest = foldr1 shorter

        -- True if we're not bumping in the direction we're travelling
        backwardBump :: Position -> Bool
        backwardBump x = relativeV `dot` x >= 0

        bumps1 :: Double -> Double -> Double -> Double -> [Double]
        bumps1 x1 w1 x2 w2 = [ (x1 + w1) - x2, x1 - (x2 + w2) ]

type Collision = (Bool, Bool) -- ^ Overlap on low and high object borders, respectively.

-- | Get collision data in all dimensions
-- Returns Nothing if the objects do not collide
collision :: GameObject -> GameObject -> Maybe (Vector Collision)
collision g1 g2 = sequenceA $ collision1 <$> posn g1 <*> size g1 <*> posn g2 <*> size g2
    where
        -- 1D collision info
        collision1 :: Double -> Double -> Double -> Double -> Maybe Collision
        collision1 x1 w1 x2 w2 = (x1 <= x2, x1 + w1 >= x2 + w2) `mcond` colliding x1 w1 x2 w2

        colliding :: Double -> Double -> Double -> Double -> Bool
        colliding x1 w1 x2 w2 =  x1 + w1 >= x2
                              && x2 + w2 >= x1

-- | If the objects collide, call the appropriate handlers; otherwise just return
tryCollide :: GameObject -- ^ Object to update
           -> GameObject -- ^ Object it collided with
           -> GameObject -- ^ Updated object
tryCollide = maybe $$ const $* collisionHandler $* collision

-- | One advancement of physics
step :: DeltaT -> GameObject -> GameObject
step t = updateVcty . updatePosn
    where
        updatePosn g = g { posn = posn g <&> (+) <*> fmap (*t) (vcty g) }

        updateVcty b@(Block { vcty = v }) = b { vcty = gravity $ v }
        updateVcty g = g

        a_g = Vector 0 (-9.81)
        gravity v = v <&> (+) <*> (a_g <&> (*t))

updatePhysics :: DeltaT -> GameState -> GameState
updatePhysics t g = g { objects = step t <$> objects g }

updateInputs :: [Input] -> GameState -> GameState
updateInputs is g = g { objects = foldr updateInput (objects g) is }
    where
        -- Update a list of gameobjects with an input
        updateInput :: Input -> [GameObject] -> [GameObject]
        updateInput (Move d) = fmap $ bool <*> moveObj d <*> isPlatform
        updateInput _ = id

        moveObj :: Direction -> GameObject -> GameObject
        moveObj d o = o { posn = moveVector d <&> (+) <*> posn o
                        }

        moveVector Up = Vector 0 0.3
        moveVector Right = Vector 0.3 0
        moveVector Down = negate <$> moveVector Up
        moveVector Left = negate <$> moveVector Right

-- | Ensure no objects are colliding
updateBumps :: GameState -> GameState
updateBumps g = g { objects = foldr bumpCons [] $ objects g }
    where
        -- | Cons the object on to the list, as well as bump every other object with it.
        bumpCons obj = (\(x, l) -> x : l) . foldCons collide2 obj
        collide2 o1 o2 = (tryCollide o1 o2, tryCollide o2 o1)

update :: [Input] -> DeltaT -> GameState -> GameState
update is t = updateBumps . updateInputs is . updatePhysics t
