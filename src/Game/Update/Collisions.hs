-- | Update game state's physical interactions
module Game.Update.Collisions ( update
                              , Collisions
                              ) where

import Prelewd

import Impure

import Num.Nonfinite
import Num.Positive
import Storage.Map hiding (difference)
import Storage.Member
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

setSeveral :: Foldable t => a -> t Dimension -> Vector a -> Vector a
setSeveral x = flip $ foldr (`setV` x)

-- | Run both objects' collision functions
collideBoth :: Time -> (ID, ID) -> Set Dimension -> GameState -> GameState
collideBoth t (i1, i2) dims g = object' (collide o2) i1
                              $ object' (collide o1) i2
                              $ g
    where
        o1 = object i1 g
        o2 = object i2 g
        collide = phys' (vcty' $ collideSet $ inelastic o1 o2) .$ applyFriction t dims

        -- | Set velocity in the collision dimensions
        collideSet :: Velocity -> Velocity -> Velocity
        collideSet = liftA3 (iff . (`elem` dims)) dimensions

inelastic :: GameObject -> GameObject -> Velocity
inelastic o1 o2 = let m1 = num $ mass $ phys o1
                      m2 = num $ mass $ phys o2
                  in term m1 m2 (vcty $ phys o1) + term m2 m1 (vcty $ phys o2)
    where
        term m1 m2 = map $ map toFinite . (/ Unit (unitless $ 1 + m2/m1)) . map Finite

        toFinite Infinite = error "Infinite is not finite"
        toFinite (Finite x) = x

-- | Advance a game state based on collisions
update :: Time -> Collisions -> GameState -> GameState
update t cs g = foldrWithKey (collideBoth t . tuple) g cs

applyFriction :: Time -> Set Dimension -> GameObject -> GameObject -> GameObject
applyFriction t dims collidee obj = let 
                                        moveDims = toSet dimensions `difference` dims
                                        norm = setSeveral 0 moveDims $ accl $ phys obj
                                        a = Unit $ fromDouble $ magnitude $ toDouble <$> norm
                                        f = ((*) `on` mu.phys) collidee obj &* a
                                    in phys' (vcty' $ friction $ num t &* f) obj

friction :: Speed -> Velocity -> Velocity
friction s = liftA2 magSub <*> map ((&* s) . Unit . fromDouble) . normalize . map toDouble
    where
        magSub x y = Unit (unitless $ signum x) &* max 0 (abs x - abs y)
