{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Transform one physics state to the next
module Game.Update.Physics ( update
                           ) where

import Prelewd hiding (filter)

import Impure

import Control.Arrow
import Data.Tuple
import Storage.Pair
import Storage.Map
import Storage.Set
import Subset.Num

import Game.Movement
import Game.Object
import Game.Physics
import Game.State
import Game.Vector
import Game.Update.Collisions hiding (update)
import Physics.Types
import Util.Unit

-- | Put each component in its own vector, in the correct location
isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions


-- | Rotate a function's argument order
rotateL :: (a -> b -> c -> d) -> b -> c -> a -> d
rotateL f y z x = f x y z

-- | Advance all physics by a certain amount of time
update :: Time -> GameState -> (Collisions, GameState)
update t = foldr updateID <$> (mempty, ) <*> keys . objects
    where
        updateID id (cs, g) = swap $ updateWCollisions t id (object id g) cs g

updateWCollisions :: Time -> ID -> GameObject -> Collisions -> GameState -> (GameState, Collisions)
updateWCollisions t id obj cs g = (rotateL $ object' . \x _->x) id g *** addCollides $ update1 t g id obj
    where
        addCollides = unionWith checkEqual cs . mapKeys (Pair id)
        checkEqual x = assert =<< (== x)

-- | Update a single object's physics
update1 :: Time -> GameState -> ID -> GameObject
                -> (GameObject, Map ID (Set Dimension))    -- ^ (object, collisions)
update1 t s id = updatePosn . phys' updateVcty
    where
        updateVcty p = p { vcty = vcty p + ((fromNat t &*) <$> accl p) }
        updatePosn obj = foldr moveAndCollide (obj, mempty) $ isolate 0 $ vcty $ phys obj

        moveAndCollide mv (obj, allCollides) = let physLookup = phys <$> objects s
                                                   shift = (fromNat t &*) <$> mv
                                                   (deltaP, collides) = move shift id (phys obj) $ physLookup
                                               in ( makeMove deltaP obj
                                                  , allCollides <> filter (not.null) collides
                                                  )

        makeMove :: Position -> GameObject -> GameObject
        makeMove = phys' . posn' . (+)
