-- | Game world tranformations over time
module Game.Update ( update
                   ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Text.Show

import Util.Impure

import Game.Collision
import Game.Input
import Game.Movement
import Game.Object
import Game.ObjectGroup
import Game.Physics
import Game.State
import Game.Vector

updateInputs :: [Input] -> GameState -> GameState
updateInputs is = objects' updateObjInputs
    where
        updateObjInputs objs = foldr updateInputAll objs is
        updateInputAll i objs = val' (objInput i) <$> objs

extract :: ID -> ObjectGroup -> Maybe (UniqueObject, ObjectGroup)
extract _ [] = Nothing
extract i (obj:objs) = if i == id obj
                       then Just (obj, objs)
                       else ((obj:) <$>)<$> extract i objs

isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions

-- | One advancement of physics
updateObjPhysics :: Time -> ObjectGroup -> GameObject -> (GameObject, ObjectGroup)
updateObjPhysics t others = updatePosn others . phys' updateVcty
    where
        updateVcty p = p { vcty = vcty p + (realToFrac . (t*) . realToFrac <$> accl p) }
        updatePosn objs obj = unify $ mapAccumR moveAndCollide (obj, objs) $ isolate 0 $ vcty $ phys obj

        unify ((obj, objs), vs) = (phys' (vcty' $ const $ foldr (liftA2 (+)) (pure 0) vs) obj, objs)
        moveAndCollide (obj, objs) v = let (shiftV, collides, dims) = move (realToFrac . (t*) . realToFrac <$> v) (phys obj) $ val' phys <$> objs
                                       in (enactCollides obj objs shiftV collides, foldr (`setV` 0) v dims)

        enactCollides :: GameObject -> ObjectGroup -> Position -> [ID] -> (GameObject, ObjectGroup)
        enactCollides obj objs shiftV collides = foldr findAndHit (makeMove shiftV obj, objs) collides
        
        findAndHit i (obj, objs) = maybe (error $ "Couldn't find object " ++ show i) (collideCons obj) $ extract i objs
        collideCons o1 (o2, objs) = mutualCollide o1 o2 <&> (:objs)

        makeMove :: Position -> GameObject -> GameObject
        makeMove = phys' . posn' . liftA2 (+)

        mutualCollide o1 o2 = (o1 `collide` val o2, val' (`collide` o1) o2)

updatePhysics :: Time -> GameState -> GameState
updatePhysics t = foldr tryUpdate <*> fmap id . objects
    where
        tryUpdate i g = fromMaybe g $ objPhysWrapper <$> object g i <*> deleteObj i g

        objPhysWrapper obj g = patch g $ updateObjPhysics t (objects g) obj

        patch :: GameState -> (GameObject, ObjectGroup) -> GameState
        patch g (obj, objs) = addObject obj $ g { objects = objs }

-- | One update "tick"
update :: [Input] -> Time -> GameState -> GameState
update is t g = foldr ($) g
                    [ updateInputs is
                    , updatePhysics t
                    ]
