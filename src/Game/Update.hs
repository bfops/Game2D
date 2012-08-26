-- | Game world tranformations over time
module Game.Update ( update
                   ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Data.List (union)
import Data.Tuple

import Util.Impure
import Util.Map hiding (update, union)

import Game.Collision
import Game.Input
import Game.Movement
import Game.Object
import Game.ObjectGroup
import Game.Physics
import Game.State
import Game.Vector

updateInputs :: [Input] -> GameState -> GameState
updateInputs is = foldr updateObjInputs <*> fmap id . objects
    where
        updateObjInputs i g = foldr (updateObjInput i) g is
        updateObjInput i = fromMaybe (error "Could not find object") .$ object' i . objInput

isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions

setSeveral :: a -> [Dimension] -> Vector a -> Vector a
setSeveral x = flip $ foldr (`setV` x)

combine :: (Foldable t, Eq a) => t [a] -> [a]
combine = foldl union []

-- | One advancement of physics
updateObjPhysics :: Time -> GameState -> GameObject -> (GameObject, [ID])
updateObjPhysics t others orig = updatePosn $ phys' updateVcty orig
    where
        updateVcty p = p { vcty = vcty p + ((t*) <$> accl p) }
        updatePosn obj = unify $ foldr moveAndCollide (obj, []) $ isolate 0 $ vcty $ phys obj

        unify (obj, collides) = (obj, collides)
        moveAndCollide mv (obj, allCollides) = let (deltaP, collides) = move ((t*) <$> mv) (phys obj) $ val' phys <$> objects others
                                               in ( phys' (vcty' $ setSeveral 0 $ combine collides) $ makeMove deltaP obj
                                                  , allCollides `union` keys collides
                                                  )

        makeMove :: Position -> GameObject -> GameObject
        makeMove = phys' . posn' . (+)

updatePhysics :: Time -> GameState -> GameState
updatePhysics t = foldr tryUpdate <*> fmap id . objects
    where
        tryUpdate i g = fromMaybe g $ objPhysWrapper <$> object g i <*> deleteObj i g

        objPhysWrapper obj g = patch g $ updateObjPhysics t g obj

        patch :: GameState -> (GameObject, [ID]) -> GameState
        patch g (obj, objs) = uncurry addObject $ foldr collideID (obj, g) objs

collideID :: ID -> (GameObject, GameState) -> (GameObject, GameState)
collideID i (obj, g) = fromMaybe (error "Can't find object") $ withObject i mutualCollide g
    where
        mutualCollide obj2 = (obj `collide` obj2, obj2 `collide` obj)

withObject :: ID -> (GameObject -> (a, GameObject)) -> GameState -> Maybe (a, GameState)
withObject i f g = do (x, obj) <- f <$> object g i
                      (x,) <$> object' i (const obj) g

-- | One update "tick"
update :: [Input] -> Time -> GameState -> GameState
update is t g = foldr ($) g
                    [ updateInputs is
                    , updatePhysics t
                    ]
