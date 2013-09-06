{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Transform game from one state to the next
module Game.Update ( game
                   ) where

import Summit.Control.Stream
import Summit.Prelewd
import Summit.Data.Id
import Summit.Data.Map
import Summit.Data.Pair

import Data.Tuple

import Game.Input
import Game.Object
import Game.Physics
import Game.State
import Game.State.Init
import Game.Update.Collisions as Collisions
import Game.Update.Input as Input
import Physics.Types

-- | Advance the game state
game :: Stream Id (Inputs, Time) GameState   
game = folds (barr updateStep) initState
    where
        updateStep (ins, t) g = let (colisns, g') = foldr (updateObject t)
                                                          (mempty, g)
                                                          (keys $ objects g)
                                in Input.update ins
                                 $ foldrWithKey setIdVcty g'
                                 $ Collisions.update colisns
                                 $ objects g' <&> fst <&> phys

        setIdVcty i v = object' (map2 $ phys' $ vcty' $ \_-> v) i

updateObject :: Time
             -> ID
             -> (Map (Pair ID) Collisions, GameState)
             -> (Map (Pair ID) Collisions, GameState)
updateObject t i (colisns, g) = let
            obj = object i g
            Id ((colisns', obj'), s) = snd obj $< ObjectInputs
                                    { worldBounds = bounds g
                                    , dt          = t
                                    , allObjects  = fst <$> objects g
                                    , objId       = i
                                    , setVcty     = lookup i (objects g <&> fst <&> phys <&> vcty)
                                                <?> vcty (phys $ fst obj)
                                    }
        in (colisns <> map2 (Pair i) colisns', object' (\_-> (obj', s)) i g)
