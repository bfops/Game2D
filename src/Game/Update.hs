{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Transform game from one state to the next
module Game.Update ( Game.Update.update
                   ) where

import Prelewd

import Data.Tuple
import Storage.Map

import Game.Input
import Game.Object
import Game.Physics
import Game.State
import Game.Update.Collisions as Collisions
import Game.Update.Input as Input
import Game.Update.Physics as Physics
import Physics.Types

-- | Advance the game state
update :: Map Input (Maybe Time)    -- ^ Currently-pressed inputs. If Time is Nothing,
                                    -- then the input has just been pushed. Otherwise, it's the hold time.
       -> Time                      -- ^ Time elapsed for this update step
       -> GameState
       -> GameState
update is t g = foldr ($) g
        [ Input.update is
        , uncurry (Collisions.update t)
        . Physics.update t
        -- Wraparound in every dimension, based on the world bounds
        , player' . phys' . posn' . liftA2 wraparound . bounds >>= ($)
        ]

wraparound :: (Distance, Distance) -> Distance -> Distance
wraparound (start, end) s = start + ((s - start) `mod` (end - start))
