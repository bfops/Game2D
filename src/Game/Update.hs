{-# LANGUAGE TupleSections #-}
-- | Transform game from one state to the next
module Game.Update ( game
                   ) where

import Data.Conduit
import Data.HashMap.Strict as HashMap
import Data.Tuple

import Game.Input
import Game.Object
import Game.Physics (vcty)
import Game.State
import Game.State.Init
import Game.Update.Input as Input
import Game.Vector (component)
import Physics.Types
import Util.Graph
import Util.ID

-- | Advance the game state
game :: Bounds -> Stream Id (Inputs, Time) GameState
game bounds = folds (barr updateStep) initState
    where
        updateStep (ins, t) = updateObjects bounds t
                          >>> Input.update ins

updateObjects :: Bounds
              -> Time
              -> GameState
              -> GameState
updateObjects bounds t g = fst $ foldl' updateObject (g, mempty) (keys $ named g)
  where
    updateObject (g', cs) i = let Id ((objs, cs'), s) = snd (call i g') $< ObjectInputs
                                                          { worldBounds = bounds
                                                          , dt          = t
                                                          , allObjects  = fst <$> g'
                                                          , collisions  = cs
                                                          }
                              in ( call' (map $ \_-> s) i $ updateNamed (\o -> (o,) . snd) objs g'
                                 , assert' (mapWithEdgesParents (speedShare $ phys <$> objs) >>> and)
                                 $ assert' symmetric
                                 $ cs'
                                 )

    speedShare ps i = all $ \((d, _), p) -> ((==) `on` component d . vcty . (`call` ps)) i p

    symmetric = graphData
            >>> \m -> m
                  $>> mapWithKey (\parent s -> toList s >>= \(e, child) -> [(child, [(e, parent)])])
                  >>> concat
                  >>> fromListWith (<>)
                  >>> (== m)
