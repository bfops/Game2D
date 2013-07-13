{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Transform one physics state to the next
module Game.Update.Physics ( update
                           ) where

import Prelewd hiding (filter)

import Storage.Map
import Subset.Num

import Game.Movement
import Game.Physics
import Game.Object
import Game.Vector
import Physics.Types
import Util.Unit

modPhys :: (Physics -> (r, Physics)) -> GameObject -> (r, GameObject)
modPhys f obj = f (phys obj) <&> (\p -> phys' (\_-> p) obj)

-- | Put each component in its own vector, in the correct location
isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions

-- | Update a single object's physics
update :: Time
       -> Map ID Physics            -- ^ Other objects
       -> ID
       -> GameObject
       -> (Map ID Collisions, GameObject)
update t others i = modPhys $ updateVcty >>> updatePosn
    where
        updateVcty p = p { vcty = vcty p + ((fromNat t &*) <$> accl p) }
        updatePosn p = foldr moveAndCollide (mempty, p) $ isolate 0 $ vcty p

        moveAndCollide mv (allCollides, p) = let
                    shift = (fromNat t &*) <$> mv
                    (deltaP, collides) = move shift i p others
                in ( allCollides <> filter (not.null) collides
                   , makeMove deltaP p
                   )

        makeMove :: Position -> Physics -> Physics
        makeMove = posn' . (+)
