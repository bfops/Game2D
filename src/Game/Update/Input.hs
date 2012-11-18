-- | Update game state to respond to input
module Game.Update.Input ( update
                         ) where

import Prelewd

import Num.Positive
import Storage.Map as Map

import Game.Input
import Game.Physics
import Game.Object
import Game.State
import Game.Vector
import Util.Unit

import Config

pushActions :: Map Input (GameState -> GameState)
pushActions = fromList $
        [ (Jump, player' $ addVcty jumpVcty)
        , (Reset, const initState)
        ]

holdActions :: Map Input (Time -> GameState -> GameState)
holdActions = fromList $
        [ (Left, const $ player' $ walk $ negate $ num moveSpeed)
        , (Right, const $ player' $ walk $ num moveSpeed)
        ]

update :: Map Input (Maybe Time) -> GameState -> GameState
update = flip $ foldrWithKey (try .$ updateInput)

-- | Use one input to update
updateInput :: Input -> Maybe Time -> Maybe (GameState -> GameState)
updateInput i Nothing = lookup i pushActions
updateInput i (Just t) = lookup i holdActions <&> ($ t)

jumpVcty :: Velocity
jumpVcty = singleV 0 Height $ num jumpSpeed

addVcty :: Velocity -> GameObject -> GameObject
addVcty = phys' . vcty' . (+)

walk :: Speed -> GameObject -> GameObject
walk v = phys' $ vcty' $ component' Width $ cap speedCap . (+ v)

cap :: Positive Speed -> Speed -> Speed
cap c v = Unit (unitless $ signum v) &* min (num c) (abs v)

try :: Maybe (a -> a) -> a -> a
try f x = (f <&> ($ x)) <?> x
