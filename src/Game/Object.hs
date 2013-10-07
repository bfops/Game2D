{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( GameObject (..)
                   , Bounds
                   , ObjectInputs (..)
                   , ObjectBehavior
                   , phys'
                   , isPlatform
                   , isBlock
                   , isPlayer
                   , player
                   ) where

import Summit.Control.Stream
import Summit.Data.Id
import Summit.Data.Map
import Summit.Impure
import Summit.Prelewd
import Summit.Template.MemberTransformer

import Data.Tuple
import Text.Show

import Game.Physics
import Game.Vector
import Physics.Types
import Util.ID

-- | An object in the game world
data GameObject
        = Block    { phys :: Physics }
        | Platform { phys :: Physics }
        | Player   { phys :: Physics }
    deriving Show

-- | Edges of the game world
type Bounds = Vector (Distance, Distance)

data ObjectInputs = ObjectInputs
        { worldBounds       :: Bounds
        , dt                :: Time
        , allObjects        :: Named GameObject
        , objId             :: ID
        , setVcty           :: Velocity                  -- ^ Set Velocity to this.
        } deriving (Show)

type ObjectBehavior = Stream Id ObjectInputs (Map ID Collisions, GameObject)

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

player :: Named GameObject -> ID
player n = fst $ find (isPlayer . snd) (assocs $ named n) <?> error "Couldn't find player"
