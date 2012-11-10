-- | Functions for dealing with game state
module Game.State ( GameState
                  , Bounds
                  , bounds
                  , objects
                  , object
                  , object'
                  , player
                  , player'
                  , inputs
                  , inputs'
                  , addObject
                  , deleteObj
                  , emptyState
                  ) where

import Prelewd hiding (filter)

import Impure

import Data.Maybe (listToMaybe)
import Storage.Map
import Text.Show

import Game.Input
import Game.Physics
import Game.Object
import Game.Vector

-- | Edges of the game world
type Bounds = Vector (Distance, Distance)

-- | Game state structure
data GameState = GameState Bounds [ID] ObjectGroup (Map Input Time)

-- Infinite lists of available IDs don't play nicely with derived Show
instance Show GameState where
    show g = "GameState {objects = " <> show (objects g) <> "}"

-- | Border around the world
bounds :: GameState -> Bounds
bounds (GameState b _ _ _) = b

-- | Get the objects in the game
objects :: GameState -> ObjectGroup
objects (GameState _ _ objs _) = objs

-- | Get information about the current input state
inputs :: GameState -> Map Input Time
inputs (GameState _ _ _ is) = is

-- | Fetch a specific object
object :: ID -> GameState -> GameObject
object id g = lookup id (objects g) <?> error ("Couldn't find object " <> show id)

-- | Update an object
object' :: (GameObject -> GameObject) -> ID -> GameState -> GameState
object' f id g = maybe (error $ "Couldn't update object " <> show id) ((`objects'` g) . const)
               $ modify (Just . f) id $ objects g

objects' :: (ObjectGroup -> ObjectGroup) -> GameState -> GameState
objects' f (GameState b x objs y) = GameState b x (f objs) y

ids' :: ([ID] -> [ID]) -> GameState -> GameState
ids' f (GameState b is x y) = GameState b (f is) x y

player :: GameState -> ID
player = (<?> error "No player!") . listToMaybe . keys . filter isPlayer . objects

player' :: (GameObject -> GameObject) -> GameState -> GameState
player' f = object' f =<< player

-- | Transform information about the current input state
inputs' :: (Map Input Time -> Map Input Time) -> GameState -> GameState
inputs' f (GameState b x y ins) = GameState b x y (f ins)

-- | Add an object into the game
addObject :: GameObject -> GameState -> GameState
addObject obj (GameState b (id:is) objs ins)
             = GameState b     is (insert id  obj objs) ins
addObject _ _ = error "Ran out of IDs"

-- | Try to remove an object
deleteObj :: ID -> GameState -> GameState
deleteObj id s = maybe (error $ "Object " <> show id <> " doesn't exist") (\o -> objects' (const o) $ ids' (id:) s)
               $ delete id $ objects s

-- | Game state with nothing in it
emptyState :: Bounds -> GameState
emptyState b = GameState b [0..] mempty mempty
