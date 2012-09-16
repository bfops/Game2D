module Game.Update.Collisions ( update
                              , Collisions
                              ) where

import Control.Arrow
import qualified Data.Set as Set
import qualified Data.Map as Map

import Game.Physics
import Game.Object
import Game.ObjectGroup hiding (id)
import Game.State
import Game.Vector
import Util.Prelewd hiding (partition)
import Util.Unit

type Collisions = Map.Map (Set.Set ID) (Set.Set Dimension)

toSet :: (Foldable t, Ord a) => t a -> Set.Set a
toSet = Set.fromList . toList

collide :: Time -> Set.Set Dimension -> GameObject -> GameObject -> GameObject
collide t dims collidee = foldr (.) id
            [ applyFriction t dims collidee
            , phys' $ vcty' $ setSeveral 0 dims
            ]

setSeveral :: Foldable t => a -> t Dimension -> Vector a -> Vector a
setSeveral x = flip $ foldr (`setV` x)

updateCollision :: Time -> (ID, ID) -> Set.Set Dimension -> GameState -> GameState
updateCollision t (i1, i2) dims g = object' (collide t dims $ object i2 g) i1
                                  $ object' (collide t dims $ object i1 g) i2
                                  $ g

update :: Time -> Collisions -> GameState -> GameState
update t cs g = Map.foldrWithKey (updateCollision t . (Set.findMin &&& Set.findMax)) g cs

applyFriction :: Time -> Set.Set Dimension -> GameObject -> GameObject -> GameObject
applyFriction t dims collidee obj = let 
                                        moveDims = toSet dimensions Set.\\ dims
                                        norm = setSeveral 0 moveDims $ accl $ phys obj
                                        f = ((+) `on` mu.phys) collidee obj * accel (magnitude $ fromAccel <$> norm)
                                    in phys' (vcty' $ friction $ t * f) obj

friction :: Speed -> Velocity -> Velocity
friction s = liftA2 magSub <*> fmap ((s *) . scalar) . normalize . fmap fromSpeed
    where
        magSub x y = scalar (fromSpeed $ signum x) * max 0 (abs x - abs y)

