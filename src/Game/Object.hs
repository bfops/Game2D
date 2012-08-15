module Game.Object ( GameObject (..)
                   , phys'
                   , isPlatform
                   , isBlock
                   , isPlayer
                   ) where

import Prelude ()
import Util.Prelewd

import Text.Show

import Game.Physics

-- | An object in the game world
data GameObject
        = Block    { phys :: Physics }
        | Platform { phys :: Physics }
        | Player   { phys :: Physics }
    deriving Show

phys' :: (Physics -> Physics) -> GameObject -> GameObject
phys' f g = g { phys = f (phys g) }

isBlock :: GameObject -> Bool
isBlock (Block {}) = True
isBlock _ = False

isPlatform :: GameObject -> Bool
isPlatform (Platform {}) = True
isPlatform _ = False

isPlayer :: GameObject -> Bool
isPlayer (Player {}) = True
isPlayer _ = False
