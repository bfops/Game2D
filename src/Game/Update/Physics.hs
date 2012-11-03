-- | Transform one physics state to the next
module Game.Update.Physics ( update
                           ) where

import Prelewd hiding (filter)

import Control.Arrow
import Data.Tuple

import Game.Movement
import Game.Object
import Game.Physics
import Game.State
import Game.Vector
import Game.Update.Collisions hiding (update)
import Impure
import Storage.Pair
import Storage.Map
import Storage.Set

-- | Put each component in its own vector, in the correct location
isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions

-- | Update a single object's physics
update1 :: Time -> GameState -> ID -> GameObject
                -> (GameObject, Map ID (Set Dimension))    -- ^ (object, collisions)
update1 t s i = updatePosn . phys' updateVcty
    where
        updateVcty p = p { vcty = vcty p + ((t*) <$> accl p) }
        updatePosn obj = foldr moveAndCollide (obj, mempty) $ isolate 0 $ vcty $ phys obj

        moveAndCollide mv (obj, allCollides) = let physLookup = phys <$> objects s
                                                   (deltaP, collides) = move ((t*) <$> mv) i (phys obj) $ physLookup
                                               in ( makeMove deltaP obj
                                                  , allCollides <> filter (not.null) collides
                                                  )

        makeMove :: Position -> GameObject -> GameObject
        makeMove = phys' . posn' . (+)

-- | Rotate a function's argument order
rotateL :: (a -> b -> c -> d) -> b -> c -> a -> d
rotateL f y z x = f x y z

-- | Advance all physics by a certain amount of time
update :: Time -> GameState -> (Collisions, GameState)
update t = foldr updateID <$> (mempty, ) <*> keys . objects
    where
        updateID i (cs, g) = swap $ updateWCollisions t i (object i g) cs g

updateWCollisions :: Time -> ID -> GameObject -> Collisions -> GameState -> (GameState, Collisions)
updateWCollisions t i obj cs g = (rotateL $ object'.const) i g *** addCollides $ update1 t g i obj
    where
        addCollides = unionWith checkEqual cs . mapKeys (Pair i)
        checkEqual x = assert =<< (== x)
