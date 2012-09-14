module Game.Update.Input ( update
                         ) where

import Util.Prelewd hiding (id, empty, lookup)

import Control.Arrow
import Data.List as List (delete)
import Data.Map as Map hiding (foldr, mapMaybe, filter, update)

import Config

import Game.Input
import Game.Physics
import Game.Object
import Game.ObjectGroup
import Game.State
import Game.Vector
import Util.Impure
import Wrappers.Events

data InputAction = Push Input (GameState -> GameState)
                 | Hold Input (Time -> GameState -> GameState) -- ^ First parameter is hold time

cmd :: InputAction -> Input
cmd (Push inp _) = inp
cmd (Hold inp _) = inp

isPush :: InputAction -> Bool
isPush (Push _ _) = True
isPush _ = False

isHold :: InputAction -> Bool
isHold (Hold _ _) = True
isHold _ = False

actionUpdate :: Time -> InputAction -> GameState -> GameState
actionUpdate _ (Push _ f) = f
actionUpdate t (Hold _ f) = f t

actions :: [InputAction]
actions = [ Push Jump $ player' $ addVcty $ jumpVcty
          , Push Left $ player' $ addVcty $ negate moveVcty
          , Push Right $ player' $ addVcty moveVcty
          ]

update :: [(Input, ButtonState)] -> Time -> GameState -> GameState
update ins dt g = let (ins', pushes) = foldr perpetuate (inputs g, []) ins
                      updates = mapMaybe getPushAction pushes ++ mapMaybe getHoldAction (assocs ins')
                  in foldr ($) (inputs' (const ins') g) updates
    where
        getPushAction :: Input -> Maybe (GameState -> GameState)
        getPushAction i = actionUpdate dt <$> find ((i ==) . cmd) pushActions

        getHoldAction :: (Input, Time) -> Maybe (GameState -> GameState)
        getHoldAction (i, t) = actionUpdate t <$> find ((i ==) . cmd) holdActions

        perpetuate (i, Press) = insert i 0 *** (i:)
        perpetuate (i, Release) = Map.delete i *** List.delete i

pushActions, holdActions :: [InputAction]
pushActions = filter isPush actions
holdActions = filter isHold actions

moveVcty, jumpVcty :: Velocity
moveVcty = assert (moveSpeed >= 0) $ singleV 0 Width moveSpeed
jumpVcty = assert (jumpSpeed >= 0) $ singleV 0 Height jumpSpeed

player' :: (GameObject -> GameObject) -> GameState -> GameState
player' = object' <&> (=<< getPlayer)
    where
        getPlayer = id . fromMaybe (error "No player!") . find (isPlayer . val) . objects

addVcty :: Velocity -> GameObject -> GameObject
addVcty = phys' . vcty' . (+)