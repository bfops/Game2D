-- | Functions for dealing with game state
module Game.State ( GameState
                  , objects
                  , object
                  , object'
                  , inputs
                  , inputs'
                  , addObject
                  , deleteObj
                  , emptyState
                  ) where

import Util.Prelewd hiding (id, empty)

import Data.Map hiding (update)
import Text.Show

import Game.Input
import Game.Physics
import Game.Object
import Game.ObjectGroup
import Util.Impure

-- | Game state structure
data GameState = GameState [ID] ObjectGroup (Map Input Time)

-- Infinite lists of available IDs don't play nicely with deriving Show
instance Show GameState where
    show g = "GameState {objects = " <> show (objects g) <> "}"

-- | Get the objects in the game
objects :: GameState -> ObjectGroup
objects (GameState _ objs _) = objs

-- | Get information about the current input state
inputs :: GameState -> Map Input Time
inputs (GameState _ _ is) = is

-- | Try to fetch a specific object
object :: ID -> GameState -> GameObject
object i g = maybe (error $ "Couldn't find object " <> show i) val $ find (id <&> (== i)) $ objects g

-- | Try to update an object
object' :: (GameObject -> GameObject) -> ID -> GameState -> GameState
object' f i g = maybe (error $ "Couldn't update object " <> show i) ((`objects'` g) . const)
              $ update f i $ objects g

objects' :: (ObjectGroup -> ObjectGroup) -> GameState -> GameState
objects' f (GameState x objs y) = GameState x (f objs) y

-- | Transform information about the current input state
inputs' :: (Map Input Time -> Map Input Time) -> GameState -> GameState
inputs' f (GameState x y ins) = GameState x y (f ins)

-- | Add an object into the game
addObject :: GameObject -> GameState -> GameState
addObject obj (GameState (i:is) objs ins)
             = GameState    is (KeyPair i obj : objs) ins
addObject _ (GameState [] _ _) = error "Ran out of IDs"

-- | Try to remove an object
deleteObj :: ID -> GameState -> GameState
deleteObj i (GameState is objs ins) = maybe (error $ "Object " <> show i <> " doesn't exist") (\o -> GameState (i:is) o ins)
                                    $ deleteBy (id <&> (== i)) objs

-- | Game state with nothing in it
emptyState :: GameState
emptyState = GameState [0..] [] empty

update :: Eq k => (v -> v) -> k -> [KeyPair k v] -> Maybe [KeyPair k v]
update _ _ [] = Nothing
update f k (x:xs) = if k == id x
                    then Just $ val' f x : xs
                    else (x:) <$> update f k xs
