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

data GameState = GameState { objects :: ObjectGroup
                           , ids     :: [ID]
                           }

-- Infinite lists of available IDs don't play nicely with deriving Show
instance Show GameState where
    show g = "GameState {objects = " ++ show (objects g) ++ "}"

objects' :: (ObjectGroup -> ObjectGroup) -> GameState -> GameState
objects' f g = g { objects = f (objects g) }

object :: GameState -> ID -> Maybe GameObject
object g i = val <$> find (id <&> (== i)) (objects g)

addObject :: GameObject -> GameState -> GameState
addObject obj (GameState { ids = (i:is), objects =                  objs })
             = GameState { ids =    is , objects = KeyPair i obj : objs }
addObject _ (GameState { ids = [] }) = error "Ran out of IDs"

deleteObj :: ID -> GameState -> Maybe GameState
deleteObj i (GameState { ids = is, objects = objs })
           = (\os -> GameState { ids = i:is, objects = os }) <$> deleteBy (id <&> (== i)) objs

emptyState :: GameState
emptyState = GameState { ids = [0..], objects = [] }
