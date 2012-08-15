module Game.Update ( update
                   ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Text.Show

import Util.Impure

import Game.Collision
import Game.Move
import Game.Physics
import Game.Types

updateInputs :: [Input] -> GameState -> GameState
updateInputs is = objects' updateObjInputs
    where
        updateObjInputs objs = foldr updateInputAll objs is
        updateInputAll i objs = val' (updateF i) <$> objs

        -- The object update function for a given input
        updateF :: Input -> GameObject -> GameObject
        updateF Jump = ifPlayer $ addVcty $ Vector 0 8
        updateF (Move d) = ifPlayer $ addVcty $ moveVcty d

        ifPlayer f obj = if' (isPlayer obj) f obj

        moveVcty Right = Vector 8 0
        moveVcty Left = negate <$> moveVcty Right
        moveVcty _ = pure 0

        addVcty :: Velocity -> GameObject -> GameObject
        addVcty v = phys' $ vcty' $ liftA2 (+) v

extract :: ID -> ObjectGroup -> Maybe (UniqueObject, ObjectGroup)
extract _ [] = Nothing
extract i (obj:objs) = if i == id obj
                       then Just (obj, objs)
                       else ((obj:) <$>)<$> extract i objs

isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (\d x -> setV d x $ pure zero) dimensions

setV :: Dimension -> a -> Vector a -> Vector a
setV d1 x = liftA2 (\d2 -> iff (d1 == d2) x) dimensions

-- | One advancement of physics
updateObjPhysics :: Time -> ObjectGroup -> GameObject -> (GameObject, ObjectGroup)
updateObjPhysics t others = updatePosn others . phys' updateVcty
    where
        updateVcty p = p { vcty = accl p <&> (*t) <&> (+) <*> vcty p }
        updatePosn objs obj = unify $ mapAccumR moveAndCollide (obj, objs) $ isolate 0 $ vcty $ phys obj

        unify ((obj, objs), vs) = (phys' (vcty' $ const $ foldr (liftA2 (+)) (pure 0) vs) obj, objs)
        moveAndCollide (obj, objs) v = let (deltaP, collides, dims) = move ((*t) <$> v) obj objs
                                       in (enactCollides obj objs deltaP collides, foldr (`setV` 0) v dims)

        enactCollides :: GameObject -> ObjectGroup -> Position -> [ID] -> (GameObject, ObjectGroup)
        enactCollides obj objs deltaP collides = foldr findAndHit (makeMove deltaP obj, objs) collides
        
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

update :: [Input] -> Time -> GameState -> GameState
update is t g = foldr ($) g
                    [ updateInputs is
                    , updatePhysics t
                    ]
