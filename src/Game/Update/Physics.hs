module Game.Update.Physics ( update
                           ) where

import Util.Prelewd hiding (id, filter)

import Data.List (deleteFirstsBy, union)
import Data.Map hiding (foldl, foldr, toList, union, update)
import Data.Tuple

import Game.Collision
import Game.Movement
import Game.Object
import Game.ObjectGroup
import Game.Physics
import Game.State
import Game.Vector

isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions

setSeveral :: a -> [Dimension] -> Vector a -> Vector a
setSeveral x = flip $ foldr (`setV` x)

combine :: (Foldable t, Eq a) => t [a] -> [a]
combine = foldl union []

deleteFirsts :: Eq a => [a] -> [a] -> [a]
deleteFirsts = deleteFirstsBy (==)

friction :: Velocity -> Velocity -> Velocity
friction = liftA2 (\d x -> signum x * max 0 (abs x - d))

-- | One advancement of physics
updateObjPhysics :: Time -> GameState -> GameObject -> (GameObject, [ID])
updateObjPhysics t s orig = updatePosn $ phys' updateVcty orig
    where
        updateVcty p = p { vcty = vcty p + ((t*) <$> accl p) }
        updatePosn obj = unify $ foldr moveAndCollide (obj, [], 0) $ isolate 0 $ vcty $ phys obj

        unify (obj, collides, a_f) = (phys' (vcty' $ friction $ (t*) <$> a_f) obj, collides)
        moveAndCollide mv (obj, allCollides, a_f) = let (deltaP, collides) = move ((t*) <$> mv) (phys obj) $ val' phys <$> objects s
                                                        dims = combine collides
                                                        invCollides = deleteFirsts (toList dimensions) <$> collides
                                                    in ( phys' (vcty' $ setSeveral 0 dims) $ makeMove deltaP obj
                                                       , allCollides `union` keys (filter (/= []) collides)
                                                       , a_f + sum (mapWithKey (addFriction obj) invCollides)
                                                       )
                                                      
        addFriction obj i dims = let scalarNorm = (realToFrac . fromAccel <$> setSeveral 0 dims (accl $ phys orig)) :: Vector Double
                                     f = ((+) `on` mu.phys) obj (object s i) * realToFrac (magnitude scalarNorm)
                                 in setSeveral f dims 0

        makeMove :: Position -> GameObject -> GameObject
        makeMove = phys' . posn' . (+)

update :: Time -> GameState -> GameState
update t = foldr tryUpdate <*> fmap id . objects
    where
        tryUpdate i g = objPhysWrapper (object g i) $ deleteObj i g

        objPhysWrapper obj g = patch g $ updateObjPhysics t g obj

        patch :: GameState -> (GameObject, [ID]) -> GameState
        patch g (obj, objs) = uncurry addObject $ foldr collideID (obj, g) objs

collideID :: ID -> (GameObject, GameState) -> (GameObject, GameState)
collideID i (obj, g) = withObject i mutualCollide g
    where
        mutualCollide obj2 = (obj `collide` obj2, obj2 `collide` obj)

withObject :: ID -> (GameObject -> (a, GameObject)) -> GameState -> (a, GameState)
withObject i f g = let (x, obj) = f $ object g i
                   in (x, object' (const obj) i g)
