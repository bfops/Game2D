{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Transform game from one state to the next
module Game.Update ( game
                   ) where

import Prelewd

import Impure

import Control.Stream
import Storage.Id
import Storage.Map
import Storage.Pair

import Game.Input
import Game.Object
import Game.Physics
import Game.State
import Game.State.Init
import Game.Update.Collisions as Collisions
import Game.Update.Input as Input
import Game.Update.Physics as Physics
import Game.Vector
import Physics.Types

-- | Edge for the game world
border :: Bounds
border = vector undefined
       $ [ (Width , (-12, 22))
         , (Height, ( -8, 12))
         ]

-- | Advance the game state
game :: Stream Id (Inputs, Time) GameState   
game = updater (barr updateStep) initState
    where
        updateStep (ins, t) g = let (colisns, g') = foldr (updateObject t)
                                                          (mempty, g)
                                                          (keys $ objects g)
                                in Input.update ins
                                 $ foldrWithKey setIdVcty g'
                                 $ Collisions.update colisns
                                 $ objects g' <&> phys

        setIdVcty i v = object' (phys' $ vcty' $ \_-> v) i

wraparound :: Bounds                -- ^ (lower, upper) dimensional bounds
           -> Position              -- ^ Position to wrap
           -> Position              -- ^ Position wrapped inside the bounds
wraparound = liftA2 $ \(start, end) s -> start + ((s - start) `mod` (end - start))

updateObject :: Time
             -> ID
             -> (Collisions.Collisions, GameState)
             -> (Collisions.Collisions, GameState)
updateObject t i (colisns, g) = let
            obj = object i g
            (newCollisions, obj') = phys' (posn' $ wraparound border)
                                <$> Physics.update t (phys <$> objects g) i obj
        in (colisns <> map2 (Pair i) newCollisions, object' (\_-> obj') i g)
