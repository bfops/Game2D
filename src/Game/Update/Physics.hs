module Game.Update.Physics ( update
                           ) where

import Util.Prelewd hiding (id, filter)

import Control.Arrow
import Data.Tuple
import qualified Data.List as List
import qualified Data.Set as Set

import Wrappers.Map hiding (union, update)

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
combine = foldl List.union []

-- | One advancement of physics
updateObjPhysics :: Time -> GameState -> UniqueObject -> (UniqueObject, [ID])
updateObjPhysics t s = updatePosn . val' (phys' updateVcty)
    where
        updateVcty p = p { vcty = vcty p + ((t*) <$> accl p) }
        updatePosn obj = foldr moveAndCollide (obj, []) $ isolate 0 $ vcty $ phys $ val obj

        moveAndCollide mv (obj, allCollides) = let (deltaP, collides) = move ((t*) <$> mv) (phys <$> obj) $ val' phys <$> objects s
                                                   dims = combine collides
                                               in ( foldr (($) . val') obj $
                                                    [ phys' $ vcty' $ setSeveral 0 dims
                                                    , makeMove deltaP
                                                    ]
                                                  , allCollides `List.union` keys (filter (/= []) collides)
                                                  )

        makeMove :: Position -> GameObject -> GameObject
        makeMove = phys' . posn' . (+)

update :: Time -> GameState -> (Collisions, GameState)
update t = foldr tryUpdate <$> (Set.empty, ) <*> fmap id . objects
    where
        tryUpdate i (cs, g) = objPhysWrapper (KeyPair i $ object g i) cs g

        objPhysWrapper :: UniqueObject -> Collisions -> GameState -> (Collisions, GameState)
        objPhysWrapper obj cs g = addCollides (id obj) cs *** patch g $ swap $ updateObjPhysics t g obj

        patch g obj = object' (const $ val obj) (id obj) g
        addCollides i cs = Set.union cs . Set.fromList . fmap (`Set.insert` Set.singleton i)
