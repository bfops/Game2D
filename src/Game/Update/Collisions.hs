{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Update the game state using collisions info.
module Game.Update.Collisions ( update
                              ) where

import Data.HashMap.Strict as HashMap
import Data.Tuple
import Text.Show

import Game.Physics
import Game.Vector hiding (component)
import Physics.Friction
import Physics.Types
import Util.Graph
import Util.ID
import Util.Unit

-- Variables with a prefix "i" represent Impulse values
-- (i.e. some variation on Momentum).

partitionNamed :: (a -> Either b c) -> Named a -> (Named b, Named c)
partitionNamed f = named
               >>> mapEither f
               >>> nameAll *** nameAll

keepMaybe :: Dimension -> Vector a -> Vector (Maybe a)
keepMaybe dim = liftA2 (mcond . (== dim)) dimensions

absorb :: Named Physics -> (Velocity, Named (Vector Momentum))
absorb objs = let (infs, fins) = partitionNamed finiteMass $ (mass &&& vcty) <$> objs
              in case toList $ named infs of
                  vF:_ -> if all (== vF) infs
                          then
                            let iFins = relativeP vF fins
                                iInfs = infs $> negate (sum iFins) / fromInteger (length infs)
                            in (vF, iFins <> iInfs)
                          else error "Infinite masses with different speeds in a collision"
                  [] -> let mT = sum $ fromPos . fst <$> fins
                            vF = negate (sum $ relativeP 0 fins) <&> (/& mT)
                        in (vF, relativeP vF fins)
  where
    finiteMass (Infinite, v) = Left v
    finiteMass (Finite m, v) = Right (m, v)

    relativeP :: Velocity -> Named (Mass, Velocity) -> Named (Vector Momentum)
    relativeP vF = map $ \(m, v) -> (vF - v) <&> (&* fromPos m)

-- | Advance a game state based on collisions.
update :: ID -> Collisions -> Named Physics -> Named Velocity
update i cs ps = if elem i cs
                 then allDirections
                  <&> (\d -> let (vF, vs) = cs
                                        $>> filterEdges (== d)
                                        >>> tree i
                                        >>> \g -> collisionsIn (filterWithID (\i' _-> elem i' $ toList g) ps) (fst d) g
                             in (fst d, unionNamed (+) (nameAll $ fromList [(i, subtractOrZero (fst d) (vcty $ call i ps) vF)]) vs)
                      )
                  $>> fromList
                  >>> foldl' (unionNamed (+)) mempty
                  >>> \vs -> traceShow ("i, vs: " <> show (i, vs))
                           $ mapWithID (\i' -> (vcty (call i' ps) +)) vs
                 else mempty
  where
    allDirections = toList dimensions <&> (,) <*> [False, True]

    -- Subtract v2 from v1 in dimension d, setting to 0 otherwise.
    subtractOrZero d v2 v1 = keepMaybe d v1 <&> (\x y -> x <&> subtract y) <*> v2 <&> (<?> 0)

collisionsIn :: Named Physics -> Dimension -> Graph Direction ID -> (Velocity, Named Velocity)
collisionsIn ps d g = let (vF, ts) = ps
                                 $>> absorb
                                 >>> map (map $ keepMaybe d)
                      in traceShow ("d: " <> show d)
                       $ traceShow ("vF, ts: " <> show (vF, ts))
                       $ ( vF
                         , g
                       $>> mapWithParents (iFriction ts)
                       >>> foldl' (unionNamed (+)) ((<?> 0) <$$> ts)
                       >>> mapWithID (\i p -> mapInfinite (mass (call i ps) <&> fromPos <&> flip (/&) <%> p) 0)
                         )
  where
    iFriction :: Named (Vector (Maybe Momentum)) -> ID -> [ID] -> Named (Vector Momentum)
    iFriction ts i rents = let t = call i ts / fromInteger (length rents)
                               m = fromList
                                 $ rents
                                 <&> \p -> (p, muTransfer (diff 0 <$$> t) (Pair i p <&> (`call` ps)))
                           in nameAll $ insert i ((t <&> (<?> 0)) - sum m) m
