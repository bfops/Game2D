-- | Input handling
module Game.Input ( Input (..)
                  , ButtonState
                  ) where

import Text.Show

import Util.Prelewd hiding (id, empty)
import Wrappers.Events

-- | Input events understood by the game
data Input = Jump
           | Left
           | Right
    deriving (Show, Eq, Ord)
