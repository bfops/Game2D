{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Transform game from one state to the next
module Game.Update ( game
                   ) where

import Prelewd

import Control.Stream
import Data.Tuple
import Storage.Id

import Game.Input
import Game.Object
import Game.Physics
import Game.State
import Game.Update.Collisions as Collisions
import Game.Update.Input as Input
import Game.Update.Physics as Physics
import Physics.Types

import Config

-- | Advance the game state
game :: Stream Id (Inputs, Time) GameState   
game = arr Just >>> updater updateStep (Id initState)
    where
        updateStep (is, t) g = Id $ foldr ($) g
            [ Input.update is
            , uncurry Collisions.update . Physics.update t
            -- Wraparound in every dimension, based on the world bounds
            , player' . phys' . posn' . liftA2 wraparound . bounds >>= ($)
            ]

wraparound :: (Distance, Distance) -> Distance -> Distance
wraparound (start, end) s = start + ((s - start) `mod` (end - start))
