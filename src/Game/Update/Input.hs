module Game.Update.Input ( update
                         ) where

import Util.Prelewd hiding (id, empty, lookup)

import Data.Map hiding (foldr, filter, mapMaybe, update)

import Config

import Game.Input
import Game.Physics
import Game.Object
import Game.ObjectGroup
import Game.State
import Game.Vector
import Util.Impure

data InputAction = Push Input (GameState -> GameState)
                 | Hold Input (Time -> GameState -> GameState)

cmd :: InputAction -> Input
cmd (Push inp _) = inp
cmd (Hold inp _) = inp

isPush :: InputAction -> Bool
isPush (Push _ _) = True
isPush _ = False

isHold :: InputAction -> Bool
isHold (Hold _ _) = True
isHold _ = False

action :: Time -> InputAction -> GameState -> GameState
action _ (Push _ f) = f
action t (Hold _ f) = f t

update :: [Input] -> Time -> GameState -> GameState
update ins dt g = let holdIns = (dt +) <$> foldr (insert <*> fromMaybe 0 . (`lookup` inputs g)) empty ins
                      updates = ((++) `on` (`mapMaybe` assocs holdIns)) getPushAction getHoldAction
                  in foldr ($) (inputs' (const holdIns) g) updates
    where
        getPushAction :: (Input, Time) -> Maybe (GameState -> GameState)
        getPushAction (i, t) = action t <$> find ((i ==) . cmd) pushActions

        getHoldAction :: (Input, Time) -> Maybe (GameState -> GameState)
        getHoldAction (i, t) = action t <$> find ((i ==) . cmd) holdActions

actions :: [InputAction]
actions = [ Push Jump $ propelPlayer $ jumpVcty
          , Hold Left $ const $ propelPlayer $ negate moveVcty
          , Hold Right $ const $ propelPlayer moveVcty
          ]

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

propelPlayer :: Velocity -> GameState -> GameState
propelPlayer = player' . addVcty

addVcty :: Velocity -> GameObject -> GameObject
addVcty = phys' . vcty' . (+)
