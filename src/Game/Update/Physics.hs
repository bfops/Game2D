-- | Transform one physics state to the next
module Game.Update.Physics ( update
                           ) where

import Util.Prelewd hiding (id, filter)

import Control.Arrow
import Data.Tuple
import qualified Data.Set as Set
import qualified Data.Map as Map

import Game.Movement
import Game.Object
import Game.ObjectGroup
import Game.Physics
import Game.State
import Game.Vector
import Game.Update.Collisions hiding (update)
import Util.Impure

-- | Put each component in its own vector, in the correct location
isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions

updateObjPhysics :: Time -> GameState -> UniqueObject -> (UniqueObject, Map.Map ID (Set.Set Dimension))
updateObjPhysics t s = updatePosn . val' (phys' updateVcty)
    where
        updateVcty p = p { vcty = vcty p + ((t*) <$> accl p) }
        updatePosn obj = foldr moveAndCollide (obj, Map.empty) $ isolate 0 $ vcty $ phys $ val obj

        moveAndCollide mv (obj, allCollides) = let (deltaP, collides) = move ((t*) <$> mv) (phys <$> obj) $ val' phys <$> objects s
                                               in ( val' (makeMove deltaP) obj
                                                  , allCollides `Map.union` Map.filter (not.Set.null) collides
                                                  )

        makeMove :: Position -> GameObject -> GameObject
        makeMove = phys' . posn' . (+)

-- | Advance all physics by a certain amount of time
update :: Time -> GameState -> (Collisions, GameState)
update t = foldr tryUpdate <$> (Map.empty, ) <*> fmap id . objects
    where
        tryUpdate i (cs, g) = objPhysWrapper (KeyPair i $ object i g) cs g

        objPhysWrapper :: UniqueObject -> Collisions -> GameState -> (Collisions, GameState)
        objPhysWrapper obj cs g = addCollides (id obj) cs *** patch g $ swap $ updateObjPhysics t g obj

        patch g obj = object' (const $ val obj) (id obj) g
        addCollides i cs = Map.unionWith checkEqual cs . Map.mapKeys (`Set.insert` Set.singleton i)
        checkEqual x = assert =<< (== x)
