-- | Update game state to respond to input
module Game.Update.Input ( update
                         ) where

import Control.Arrow
import qualified Data.List as List
import Data.Maybe (listToMaybe)

import Config

import Game.Input
import Game.Physics
import Game.Object
import Game.State
import Game.Vector
import Impure
import Storage.Map as Map
import Prelewd
import Wrappers.Events

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
          , Push Left $ player' $ addVcty $ negate moveVcty
          , Push Right $ player' $ addVcty moveVcty
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
        perpetuate (i, Release) = ((<?>) =<< delete i) *** List.delete i

pushActions, holdActions :: [InputAction]
[pushActions, holdActions] = map puref [fromPush, fromHold 0] <&> (`mapMaybe` actions)

moveVcty, jumpVcty :: Velocity
moveVcty = assert (moveSpeed >= 0) $ singleV 0 Width moveSpeed
jumpVcty = assert (jumpSpeed >= 0) $ singleV 0 Height jumpSpeed

player' :: (GameObject -> GameObject) -> GameState -> GameState
player' = object' <&> (=<< getPlayer)
    where
        getPlayer = (<?> error "No player!") . listToMaybe . keys . Map.filter isPlayer . objects

addVcty :: Velocity -> GameObject -> GameObject
addVcty = phys' . vcty' . (+)
