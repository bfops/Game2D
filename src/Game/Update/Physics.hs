{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Transform one physics state to the next
module Game.Update.Physics ( update
                           ) where

import Prelewd hiding (filter)

import Impure

import Storage.Pair
import Storage.Map
import Storage.Set
import Subset.Num

import Game.Movement
import Game.Physics
import Game.Object
import Game.State
import Game.Vector
import Game.Update.Collisions hiding (update)
import Physics.Types
import Util.Unit

-- | `objects'` with a result
modObjects :: (ObjectGroup -> (ObjectGroup, r)) -> GameState -> (GameState, r)
modObjects f g = map2 (\objs -> objects' (\_-> objs) g) $ f $ objects g

modEntry :: Ord k => (v -> (v, r)) -> k -> Map k v -> Maybe (Map k v, r)
modEntry f k m = do v <- lookup k m
                    let (v', r) = f v
                    modify (\_-> Just v') k m <&> (, r)

modPhys :: (Physics -> (Physics, r)) -> GameObject -> (GameObject, r)
modPhys f obj = map2 (\p -> phys' (\_-> p) obj) $ f $ phys obj

-- | Put each component in its own vector, in the correct location
isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions

-- | Advance all physics by a certain amount of time
update :: Time -> GameState -> (GameState, Collisions)
update t = modObjects (foldr (updateID t) <$> (, mempty) <*> keys)

updateID :: Time -> ID -> (Map ID GameObject, Collisions) -> (Map ID GameObject, Collisions)
updateID t i (objs, colisns) = modEntry (update1 t (phys <$> objs) i >>> map addCollides) i objs
                           <?> error "updateID failed to find object"
    where
        addCollides = unionWith checkEqual colisns . map2 (Pair i)
        checkEqual x = assert =<< (== x)

-- | Update a single object's physics
update1 :: Time -> Map ID Physics -> ID -> GameObject
        -> (GameObject, Map ID (Set Dimension))    -- (object, collision dimensions)
update1 t others i = modPhys $ updateVcty >>> updatePosn
    where
        updateVcty p = p { vcty = vcty p + ((fromNat t &*) <$> accl p) }
        updatePosn p = foldr moveAndCollide (p, mempty) $ isolate 0 $ vcty p

        moveAndCollide mv (p, allCollides) = let
                    shift = (fromNat t &*) <$> mv
                    (deltaP, collides) = move shift i p others
                in ( makeMove deltaP p
                   , allCollides <> filter (not.null) collides
                   )

        makeMove :: Position -> Physics -> Physics
        makeMove = posn' . (+)
