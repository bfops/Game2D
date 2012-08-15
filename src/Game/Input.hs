-- | Input handling
module Game.Input ( Input (..)
                  , objInput
                  ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Text.Show

import Util.Impure

import Game.Object
import Game.Physics

import Config

-- | Input events understood by the game
data Input = Jump
           | Left
           | Right
    deriving (Eq, Show)

-- | Update an object according to some input
objInput :: Input -> GameObject -> GameObject
objInput Jump =  addPlayerVcty $ jumpVcty
objInput Left =  addPlayerVcty $ negate <$> moveVcty
objInput Right = addPlayerVcty $ moveVcty

moveVcty, jumpVcty :: Velocity
moveVcty = assert (moveSpeed >= 0) $ singleV 0 Width moveSpeed
jumpVcty = assert (jumpSpeed >= 0) $ singleV 0 Height jumpSpeed

-- | Add a velocity iff the object is a player
addPlayerVcty :: Velocity -> GameObject -> GameObject
addPlayerVcty v obj = if' (isPlayer obj) (addVcty v) obj

addVcty :: Velocity -> GameObject -> GameObject
addVcty = phys' . vcty' . liftA2 (+)
