-- | Transform game from one state to the next
module Game.Update ( Game.Update.update
                   ) where

import Prelewd

import Data.Tuple

import Game.Input
import Game.Object
import Game.Physics
import Game.State
import Game.Update.Collisions as Collisions
import Game.Update.Input as Input
import Game.Update.Physics as Physics

-- | Advance the game state
update :: [(Input, ButtonState)] -- ^ List of input events, in order of ascending age
       -> Time                   -- ^ Time elapsed for this step
       -> GameState
       -> GameState
update is t g = foldr ($) g
        [ Input.update is t
        , uncurry (Collisions.update t)
        . Physics.update t
        -- Wraparound in every dimension, based on the world bounds
        , player' . phys' . posn' . liftA2 wraparound . bounds >>= ($)
        ]

wraparound :: (Distance, Distance) -> Distance -> Distance
wraparound (start, end) s = start + ((s - start) `mod` (end - start))
