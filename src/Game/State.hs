-- | Functions for dealing with game state
module Game.State ( GameState
                  , objects
                  , objects'
                  , object
                  , addObject
                  , deleteObj
                  , emptyState
                  ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Text.Show

import Game.Object
import Game.ObjectGroup
import Util.Impure

-- | Game state structure
data GameState = GameState { objects :: ObjectGroup -- ^ Fetch object list from the state
                           , ids     :: [ID]
                           }

-- Infinite lists of available IDs don't play nicely with deriving Show
instance Show GameState where
    show g = "GameState {objects = " ++ show (objects g) ++ "}"

-- | Transform object group
objects' :: (ObjectGroup -> ObjectGroup) -> GameState -> GameState
objects' f g = g { objects = f (objects g) }

-- | Try to fetch a specific object
object :: GameState -> ID -> Maybe GameObject
object g i = val <$> find (id <&> (== i)) (objects g)

-- | Add an object into the game
addObject :: GameObject -> GameState -> GameState
addObject obj (GameState { ids = (i:is), objects =                 objs })
             = GameState { ids =    is , objects = KeyPair i obj : objs }
addObject _ (GameState { ids = [] }) = error "Ran out of IDs"

-- | Try to remove an object
deleteObj :: ID -> GameState -> Maybe GameState
deleteObj i (GameState { ids = is, objects = objs })
           = (\os -> GameState { ids = i:is, objects = os }) <$> deleteBy (id <&> (== i)) objs

-- | Game state with nothing in it
emptyState :: GameState
emptyState = GameState { ids = [0..], objects = [] }
