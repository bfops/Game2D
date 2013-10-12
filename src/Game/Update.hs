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
import Game.State.Init
import Game.Update.Collisions as Collisions
import Game.Update.Input as Input
import Physics.Types
import Util.ID

-- | Advance the game state
game :: Bounds -> Stream Id (Inputs, Time) (Named (GameObject, ObjectBehavior))
game bounds = folds (barr updateStep) initState
    where
        updateStep (ins, t) g = let (colisns, g') = foldr (updateObject bounds t)
                                                          (mempty, g)
                                                          (keys $ named g)
                                in Input.update ins
                                 $ foldrWithKey setIdVcty g'
                                 $ Collisions.update colisns
                                 $ named g' <&> fst <&> phys

        setIdVcty i v = call' (map2 $ phys' $ vcty' $ \_-> v) i

updateObject :: Bounds
             -> Time
             -> ID
             -> (Map (Pair ID) Collisions, Named (GameObject, ObjectBehavior))
             -> (Map (Pair ID) Collisions, Named (GameObject, ObjectBehavior))
updateObject bounds t i (colisns, g) = let
            obj = call i g
            Id ((colisns', obj'), s) = snd obj $< ObjectInputs
                                    { worldBounds = bounds
                                    , dt          = t
                                    , allObjects  = fst <$> g
                                    , setVcty     = lookup i (named g <&> fst <&> phys <&> vcty)
                                                <?> vcty (phys $ fst obj)
                                    }
        in (colisns <> map2 (Pair i) colisns', call' (\_-> (obj', s)) i g)
