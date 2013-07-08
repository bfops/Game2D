{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Update game state to respond to input
module Game.Update.Input ( update
                         ) where

import Prelewd hiding (Either (..))

import Storage.Map
import Subset.Num

import Game.Input
import Game.Object
import Game.Physics
import Game.State
import Game.State.Init
import Game.Vector
import Physics.Types

-- | Fastest you can push yourself
speedCap :: Positive Speed
speedCap = 8

-- | Velocity addition for a jump
jumpVcty :: Velocity
jumpVcty = singleV 0 Height 12

-- | Speed gained for movement
moveSpeed :: Positive Speed
moveSpeed = 0.3

pushActions :: Map Input (GameState -> GameState)
pushActions = fromList $
        [ (Jump, player' $ addVcty jumpVcty)
        , (Reset, \_-> initState)
        ]

holdActions :: Map Input (Time -> GameState -> GameState)
holdActions = fromList $
        [ (Left, \_-> player' $ walk $ negate $ fromPos moveSpeed)
        , (Right, \_-> player' $ walk $ fromPos moveSpeed)
        ]

update :: Inputs
       -> GameState
       -> GameState
update = flip $ foldrWithKey (\k v -> try ($) $ inputUpdater k v)

-- | Use one input to update
inputUpdater :: Input -> Maybe Time -> Maybe (GameState -> GameState)
inputUpdater i Nothing = lookup i pushActions
inputUpdater i (Just t) = lookup i holdActions <&> ($ t)

cap :: (Num a, Ord a) => Positive a -> a -> a
cap c v = signum v * min (fromPos c) (abs v)

addVcty :: Velocity -> GameObject -> GameObject
addVcty = phys' . vcty' . (+)

walk :: Speed -> GameObject -> GameObject
walk v = phys' $ vcty' $ component' Width $ cap speedCap . (+ v)
