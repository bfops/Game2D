{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Update game state to respond to input
module Game.Update.Input ( update
                         ) where

import Summit.Prelewd hiding (Either (..))
import Summit.Data.Map
import Summit.Subset.Num

import Data.Tuple

import Game.Input
import Game.Object
import Game.Physics
import Game.State
import Game.State.Init
import Game.Vector
import Physics.Types
import Util.ID

-- | Fastest you can push yourself
speedCap :: Positive Speed
speedCap = 8

-- | Velocity addition for a jump
jumpVcty :: Velocity
jumpVcty = singleV 0 Height 12

-- | Speed gained for movement
moveSpeed :: Positive Speed
moveSpeed = 0.3

playerV' :: (Velocity -> Velocity) -> GameState -> GameState
playerV' f n = call' (map2 $ phys' $ vcty' f) (player $ fst <$> n) n

pushActions :: Map Input (GameState -> GameState)
pushActions = fromList
            [ (Jump, playerV' (+ jumpVcty))
            , (Reset, \_-> initState)
            ]

holdActions :: Map Input (Time -> GameState -> GameState)
holdActions = fromList
            [ (Left, \_-> playerV' $ walk $ negate $ fromPos moveSpeed)
            , (Right, \_-> playerV' $ walk $ fromPos moveSpeed)
            ]

cap :: (Num a, Ord a) => Positive a -> a -> a
cap c v = signum v * min (fromPos c) (abs v)

update :: Inputs -> GameState -> GameState
update = flip $ foldrWithKey (\k v -> try ($) $ inputUpdater k v)
    where
        inputUpdater i Nothing = lookup i pushActions
        inputUpdater i (Just t) = lookup i holdActions <&> ($ t)

walk :: Speed -> Velocity -> Velocity
walk v = component' Width $ cap speedCap . (+ v)
