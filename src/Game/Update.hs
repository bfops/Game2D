module Game.Update ( Game.Update.update
                   ) where

import Game.Input
import Game.Physics
import Game.State
import Game.Update.Collisions as Collisions
import Game.Update.Input as Input
import Game.Update.Physics as Physics
import Util.Prelewd

import Data.Tuple

update :: [(Input, ButtonState)] -> Time -> GameState -> GameState
update is t = foldr (.) id
        [ Input.update is t
        , uncurry (Collisions.update t)
        . Physics.update t
        ]
