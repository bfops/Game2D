-- | Functions for dealing with game state
module Game.State ( GameState
                  , Bounds
                  , bounds
                  , bounds'
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
import Storage.List (head, tail)
import Storage.Map
import Template.MemberTransformer
import Text.Show

import Game.Input
import Game.Physics
import Game.Object
import Game.Vector

-- | Edges of the game world
type Bounds = Vector (Distance, Distance)

-- | Game state structure
data GameState = GameState { bounds :: Bounds
                           , ids    :: [ID]
                           , objs   :: ObjectGroup
                           , inputs :: Map Input Time
                           }

$(memberTransformers ''GameState)

-- Infinite lists of available IDs don't play nicely with derived Show
instance Show GameState where
    show g = "GameState {objects = " <> show (objects g) <> "}"

-- | Get the objects in the game
objects :: GameState -> ObjectGroup
objects = objs

-- | Fetch a specific object
object :: ID -> GameState -> GameObject
object id g = lookup id (objects g) <?> error ("Couldn't find object " <> show id)

-- | Update an object
object' :: (GameObject -> GameObject) -> ID -> GameState -> GameState
object' f id g = maybe (error $ "Couldn't update object " <> show id) ((`objs'` g) . const)
               $ modify (Just . f) id $ objects g

player :: GameState -> ID
player = (<?> error "No player!") . listToMaybe . keys . filter isPlayer . objects

player' :: (GameObject -> GameObject) -> GameState -> GameState
player' f = object' f =<< player

-- | Add an object into the game
addObject :: GameObject -> GameState -> GameState
addObject obj s = (do
        id <- head $ ids s
        rest <- tail $ ids s
        return $ objs' (insert id obj) $ ids' (const rest) s)
        <?> error "Ran out of ID's"

-- | Try to remove an object
deleteObj :: ID -> GameState -> GameState
deleteObj id s = maybe (error $ "Object " <> show id <> " doesn't exist") (\o -> objs' (const o) $ ids' (id:) s)
               $ delete id $ objects s

-- | Game state with nothing in it
emptyState :: Bounds -> GameState
emptyState b = GameState b [0..] mempty mempty
