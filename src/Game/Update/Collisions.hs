-- | Update game state's physical interactions
module Game.Update.Collisions ( update
                              , Collisions
                              ) where

import Prelewd

import Storage.Map hiding (difference)
import Storage.Pair
import Storage.Set

import Game.Physics
import Game.Object
import Game.State
import Game.Vector
import Util.Unit

-- | Map object interactions to their collision dimensions
type Collisions = Map (Pair ID) (Set Dimension)

toSet :: (Foldable t, Ord a) => t a -> Set a
toSet = set . toList

toDouble :: Real a => a -> Double
toDouble = realToFrac

fromDouble :: Fractional b => Double -> b
fromDouble d = let precision = 100 :: Integer
               in ((/) `on` fromIntegral) (round $ fromIntegral precision * d) precision

collide :: Time -> Set Dimension -> GameObject -> GameObject -> GameObject
collide t dims collidee = foldr (.) identity
            [ applyFriction t dims collidee
            , phys' $ vcty' $ setSeveral 0 dims
            ]

setSeveral :: Foldable t => a -> t Dimension -> Vector a -> Vector a
setSeveral x = flip $ foldr (`setV` x)

-- | Run both objects' collision functions
collideBoth :: Time -> (ID, ID) -> Set Dimension -> GameState -> GameState
collideBoth t (i1, i2) dims g = object' (collide t dims $ object i2 g) i1
                              $ object' (collide t dims $ object i1 g) i2
                              $ g

-- | Advance a game state based on collisions
update :: Time -> Collisions -> GameState -> GameState
update t cs g = foldrWithKey (collideBoth t . tuple) g cs

applyFriction :: Time -> Set Dimension -> GameObject -> GameObject -> GameObject
applyFriction t dims collidee obj = let 
                                        moveDims = toSet dimensions `difference` dims
                                        norm = setSeveral 0 moveDims $ accl $ phys obj
                                        a = accel $ fromDouble $ magnitude $ toDouble <$> norm
                                        f = ((*) `on` mu.phys) collidee obj &* a
                                    in phys' (vcty' $ friction $ t &* f) obj

friction :: Speed -> Velocity -> Velocity
friction s = liftA2 magSub <*> map ((&* s) . scalar . fromDouble) . normalize . map toDouble
    where
        magSub x y = scalar (unitless $ signum x) &* max 0 (abs x - abs y)
