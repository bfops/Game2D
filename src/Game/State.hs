-- | Functions for dealing with game state
module Game.State ( GameState
                  , objects
                  , object
                  , object'
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
data GameState = GameState [ID] ObjectGroup

-- Infinite lists of available IDs don't play nicely with deriving Show
instance Show GameState where
    show g = "GameState {objects = " ++ show (objects g) ++ "}"

objects :: GameState -> ObjectGroup
objects (GameState _ objs) = objs

-- | Try to fetch a specific object
object :: GameState -> ID -> Maybe GameObject
object g i = val <$> find (id <&> (== i)) (objects g)

object' :: ID -> (GameObject -> GameObject) -> GameState -> Maybe GameState
object' i f (GameState ids objs) = GameState ids <$> update f i objs

-- | Add an object into the game
addObject :: GameObject -> GameState -> GameState
addObject obj (GameState (i:is) objs)
             = GameState    is $ KeyPair i obj : objs
addObject _ (GameState [] _) = error "Ran out of IDs"

-- | Try to remove an object
deleteObj :: ID -> GameState -> Maybe GameState
deleteObj i (GameState is objs)
           = (GameState $ i:is) <$> deleteBy (id <&> (== i)) objs

-- | Game state with nothing in it
emptyState :: GameState
emptyState = GameState [0..] []

update :: Eq k => (v -> v) -> k -> [KeyPair k v] -> Maybe [KeyPair k v]
update _ _ [] = Nothing
update f k (x:xs) = if k == id x
                    then Just $ val' f x : xs
                    else (x:) <$> update f k xs
