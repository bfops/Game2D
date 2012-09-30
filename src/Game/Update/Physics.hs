-- | Transform one physics state to the next
module Game.Update.Physics ( update
                           ) where

import Util.Prelewd hiding (id, join, filter)

import Control.Arrow
import Data.Tuple

import Game.Movement
import Game.Object
import Game.Physics
import Game.State
import Game.Vector
import Game.Update.Collisions hiding (update)
import Util.Impure
import Util.Pair
import Util.Map
import Util.Set

-- | Put each component in its own vector, in the correct location
isolate :: a -> Vector a -> Vector (Vector a)
isolate zero = liftA2 (singleV zero) dimensions

updateObjPhysics :: Time -> GameState -> (ID, GameObject) -> ((ID, GameObject), Map ID (Set Dimension))
updateObjPhysics t s = updatePosn . fmap (phys' updateVcty)
    where
        updateVcty p = p { vcty = vcty p + ((t*) <$> accl p) }
        updatePosn obj = foldr moveAndCollide (obj, mempty) $ isolate 0 $ vcty $ phys $ snd obj

        moveAndCollide mv (obj, allCollides) = let physLookup = phys <$> objects s
                                                   (deltaP, collides) = move ((t*) <$> mv) (phys <$> obj) $ physLookup
                                               in ( fmap (makeMove deltaP) obj
                                                  , allCollides `join` filter (not.null) collides
                                                  )

        makeMove :: Position -> GameObject -> GameObject
        makeMove = phys' . posn' . (+)

-- | Advance all physics by a certain amount of time
update :: Time -> GameState -> (Collisions, GameState)
update t = foldr tryUpdate <$> (mempty, ) <*> keys . objects
    where
        tryUpdate i (cs, g) = objPhysWrapper (i, object i g) cs g

        objPhysWrapper :: (ID, GameObject) -> Collisions -> GameState -> (Collisions, GameState)
        objPhysWrapper obj cs g = addCollides (fst obj) cs *** patch g $ swap $ updateObjPhysics t g obj

        patch g obj = object' (const $ snd obj) (fst obj) g
        addCollides i cs = joinWith checkEqual cs . mapKeys (pair i)
        checkEqual x = assert =<< (== x)
