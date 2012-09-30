-- | Update game state's physical interactions
module Game.Update.Collisions ( update
                              , Collisions
                              ) where

import Game.Physics
import Game.Object
import Game.ObjectGroup hiding (id)
import Game.State
import Game.Vector
import Util.Map
import Util.Pair
import Util.Prelewd hiding (partition)
import Util.Set
import Util.Unit

-- | Map object interactions to their collision dimensions
type Collisions = Map (Pair ID) (Set Dimension)

toSet :: (Foldable t, Ord a) => t a -> Set a
toSet = set . toList

collide :: Time -> Set Dimension -> GameObject -> GameObject -> GameObject
collide t dims collidee = foldr (.) id
            [ applyFriction t dims collidee
            , phys' $ vcty' $ setSeveral 0 dims
            ]

setSeveral :: Foldable t => a -> t Dimension -> Vector a -> Vector a
setSeveral x = flip $ foldr (`setV` x)

updateCollision :: Time -> (ID, ID) -> Set Dimension -> GameState -> GameState
updateCollision t (i1, i2) dims g = object' (collide t dims $ object i2 g) i1
                                  $ object' (collide t dims $ object i1 g) i2
                                  $ g

-- | Advance a game state based on collisions
update :: Time -> Collisions -> GameState -> GameState
update t cs g = foldrWithKey (updateCollision t . tuple) g cs

applyFriction :: Time -> Set Dimension -> GameObject -> GameObject -> GameObject
applyFriction t dims collidee obj = let 
                                        moveDims = toSet dimensions `difference` dims
                                        norm = setSeveral 0 moveDims $ accl $ phys obj
                                        f = ((+) `on` mu.phys) collidee obj * accel (magnitude $ fromAccel <$> norm)
                                    in phys' (vcty' $ friction $ t * f) obj

friction :: Speed -> Velocity -> Velocity
friction s = liftA2 magSub <*> fmap ((s *) . scalar) . normalize . fmap fromSpeed
    where
        magSub x y = scalar (fromSpeed $ signum x) * max 0 (abs x - abs y)

