{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Transform one physics state to the next
module Game.Update.Physics ( update
                           ) where

import Data.HashSet
import Text.Show

import Game.Movement
import Game.Physics
import Game.Vector
import Physics.Types
import Util.Graph
import Util.ID
import Util.Unit

for' :: Foldable t => (a -> b -> b) -> t a -> b -> b
for' = flip . foldl' . flip

addNeighbors :: Ord a => ID -> Named (Set (Pair a)) -> Graph a ID
addNeighbors i = foldWithID' (for' . addEdges) mempty
  where
    addEdges i' (Pair to from) = addEdge i i' to >>> addEdge i' i from

-- | Put each component in its own vector, in the correct location
isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions

-- | Update a single object's physics
update :: ID
       -> Time                      -- ^ Delta t
       -> Named Physics             -- ^ All the objects
       -> (Collisions, Physics)
update i t objs = (call i >>> updateVcty >>> updatePosn) objs
    where
        updateVcty p = p { vcty = vcty p + ((fromNat t &*) <$> accl p)
                         }
        updatePosn p = traceShow ("vcty p: " <> show (vcty p))
                     $ foldl' moveAndCollide (mempty, p) $ (fromNat t &*) <$$> isolate 0 (vcty p)

        moveAndCollide (allCollides, p) mv = let
                    (deltaP, collides) = move mv p (unname i objs)
                in traceShow ("mv: " <> show mv)
                 $ traceShow ("collides: " <> show collides)
                 $ ( allCollides <> addNeighbors i (toEdges <$$> collides)
                   , makeMove deltaP p
                   )

        toEdges (d, b) = (d,) <$> Pair b (not b)

        makeMove :: Position -> Physics -> Physics
        makeMove = posn' . (+)
