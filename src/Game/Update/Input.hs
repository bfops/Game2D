-- | Update game state to respond to input
module Game.Update.Input ( update
                         ) where

import Prelewd

import Storage.Map as Map
import Subset.Num

import Game.Input
import Game.Physics
import Game.Object
import Game.State
import Game.Vector
import Physics.Types

import Config

cap :: (Num a, Ord a) => Positive a -> a -> a
cap c v = signum v * min (fromPos c) (abs v)

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

update :: Map Input (Maybe Time) -- ^ Input to (Just holdTime | Nothing if newly-pressed)
       -> GameState
       -> GameState
update = flip $ foldrWithKey (try .$ inputUpdater)

-- | Use one input to update
inputUpdater :: Input -> Maybe Time -> Maybe (GameState -> GameState)
inputUpdater i Nothing = lookup i pushActions
inputUpdater i (Just t) = lookup i holdActions <&> ($ t)

jumpVcty :: Velocity
jumpVcty = singleV 0 Height $ fromPos jumpSpeed

addVcty :: Velocity -> GameObject -> GameObject
addVcty = phys' . vcty' . (+)

walk :: Speed -> GameObject -> GameObject
walk v = phys' $ vcty' $ component' Width $ cap speedCap . (+ v)
