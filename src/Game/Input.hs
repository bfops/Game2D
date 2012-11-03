-- | Input handling
module Game.Input ( Input (..)
                  , ButtonState
                  ) where

import Text.Show

import Prelewd
import Wrappers.Events

-- | Input events understood by the game
data Input = Jump
           | Left
           | Right
           | Reset
    deriving (Show, Eq, Ord)
