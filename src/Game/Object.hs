{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( GameObject (..)
                   , ID
                   , Bounds
                   , phys'
                   , isPlatform
                   , isBlock
                   , isPlayer
                   ) where

import Prelewd

import Template.MemberTransformer
import Text.Show

import Game.Physics
import Game.Vector
import Physics.Types

-- | An object in the game world
data GameObject
        = Block    { phys :: Physics }
        | Platform { phys :: Physics }
        | Player   { phys :: Physics }
    deriving Show

-- | Unique identifier
type ID = Integer

-- | Edges of the game world
type Bounds = Vector (Distance, Distance)

$(memberTransformers ''GameObject)

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
