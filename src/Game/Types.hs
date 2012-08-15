module Game.Types ( Input (..)
                  , Direction(..)
                  ) where

-- | Input events understood by the game
data Input = Jump
           | Move Direction
    deriving (Eq, Show)

-- | Cardinal directions
data Direction = Up | Down | Left | Right
    deriving (Eq, Show)

