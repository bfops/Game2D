-- | Collision handling
module Game.Collision ( collide
                      , Collisions
                      ) where

import Util.Prelewd

import Data.Set

import Game.Object
import Game.ObjectGroup

type Collisions = Set (Set ID)

-- | Collision handler
collide :: GameObject       -- ^ GameObject to update
        -> GameObject       -- ^ GameObject it collided with
        -> GameObject       -- ^ Updated object
collide = const
