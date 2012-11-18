-- | Update game state to respond to input
module Game.Update.Input ( update
                         ) where

import Prelewd

import Control.Arrow
import Num.Positive
import Storage.Map as Map
import Storage.List as List

import Game.Input
import Game.Physics
import Game.Object
import Game.State
import Game.Vector
import Util.Unit
import Wrappers.Events

import Config

puref :: Functor f => (a -> f b) -> a -> f a
puref f x = x <$ f x

-- | Map an Input to a gamestate transformation
data InputAction = Push Input (GameState -> GameState)
                 | Hold Input (Time -> GameState -> GameState) -- ^ First parameter is hold time

-- | Get the input command which corresponds to this action
cmd :: InputAction -> Input
cmd (Push inp _) = inp
cmd (Hold inp _) = inp

fromPush :: InputAction -> Maybe (GameState -> GameState)
fromPush (Push _ f) = Just f
fromPush _ = Nothing

fromHold :: Time -> InputAction -> Maybe (GameState -> GameState)
fromHold t (Hold _ f) = Just $ f t
fromHold _ _ = Nothing

actionUpdate :: Time -> InputAction -> GameState -> GameState
actionUpdate _ (Push _ f) = f
actionUpdate t (Hold _ f) = f t

actions :: [InputAction]
actions = [ Push Jump $ player' $ addVcty $ jumpVcty
          , Hold Left $ const $ player' $ walk $ negate $ num moveSpeed
          , Hold Right $ const $ player' $ walk $ num moveSpeed
          , Push Reset $ const initState
          ]

-- | Advance game state to deal with user input
update :: [(Input, ButtonState)] -> Time -> GameState -> GameState
update ins dt g = let (ins', pushes) = foldr perpetuate (inputs g, []) ins
                      updates = mapMaybe getPushAction pushes <> mapMaybe getHoldAction (assocs ins')
                  in foldr ($) (inputs' (const ins') g) updates
    where
        getPushAction :: Input -> Maybe (GameState -> GameState)
        getPushAction i = actionUpdate dt <$> find ((i ==) . cmd) pushActions

        getHoldAction :: (Input, Time) -> Maybe (GameState -> GameState)
        getHoldAction (i, t) = actionUpdate t <$> find ((i ==) . cmd) holdActions

        -- Step the folded parameter with a new input
        perpetuate (i, Press) = insert i 0 *** (i:)
        perpetuate (i, Release) = ((<?>) =<< Map.delete i) *** ((<?>) =<< List.delete i)

pushActions, holdActions :: [InputAction]
[pushActions, holdActions] = map puref [fromPush, fromHold 0] <&> (`mapMaybe` actions)

jumpVcty :: Velocity
jumpVcty = singleV 0 Height $ num jumpSpeed

addVcty :: Velocity -> GameObject -> GameObject
addVcty = phys' . vcty' . (+)

walk :: Speed -> GameObject -> GameObject
walk v = phys' $ vcty' $ component' Width $ cap speedCap . (+ v)

cap :: Positive Speed -> Speed -> Speed
cap c v = Unit (unitless $ signum v) &* min (num c) (abs v)
