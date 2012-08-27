-- | Basic game object type, and associated functions
module Game.Object ( GameObject (..)
                   , phys'
                   , isPlatform
                   , isBlock
                   , isPlayer
                   ) where

import Util.Prelewd

import Text.Show

import Game.Physics

-- | An object in the game world
data GameObject
        = Block    { phys :: Physics }
        | Platform { phys :: Physics }
        | Player   { phys :: Physics }
    deriving Show

-- | Transform physics
phys' :: (Physics -> Physics) -> GameObject -> GameObject
phys' f g = g { phys = f (phys g) }

-- | Is the object a block?
isBlock :: GameObject -> Bool
isBlock (Block {}) = True
isBlock _ = False

-- | Is the object a platform?
isPlatform :: GameObject -> Bool
isPlatform (Platform {}) = True
isPlatform _ = False

-- | Is the object a player?
isPlayer :: GameObject -> Bool
isPlayer (Player {}) = True
isPlayer _ = False
