-- | Input handling
module Game.Input ( Input (..)
                  , Inputs
                  , ButtonState
                  ) where

import Data.HashMap.Strict
import Text.Show

import Physics.Types
import Wrappers.Events

-- | Input events understood by the game
data Input = Jump
           | Left
           | Right
           | Reset
    deriving (Show, Eq, Ord)

-- ^ Input to (Just hold time | Nothing if newly-pressed)
type Inputs = Map Input (Maybe Time)
