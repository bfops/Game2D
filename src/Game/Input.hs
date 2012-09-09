-- | Input handling
module Game.Input ( Input (..)
                  ) where

import Util.Prelewd hiding (id, empty)

import Text.Show

-- | Input events understood by the game
data Input = Jump
           | Left
           | Right
    deriving (Show, Eq, Ord)
