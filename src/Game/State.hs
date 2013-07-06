{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
-- | Functions for dealing with game state
module Game.State ( GameState
                  , Bounds
                  , bounds
                  , bounds'
                  , objects
                  , objects'
                  , object
                  , object'
                  , player
                  , player'
                  , addObject
                  , deleteObj
                  , emptyState
                  ) where

import Prelewd hiding (filter)

import Impure

import Storage.List (head, tail)
import Storage.Map
import Template.MemberTransformer
import Text.Show

import Game.Object
import Game.Vector
import Physics.Types

-- | Edges of the game world
type Bounds = Vector (Distance, Distance)

-- | Game state structure
data GameState = GameState { bounds :: Bounds
                           , ids    :: [ID]
                           , objs   :: ObjectGroup
                           }

$(memberTransformers ''GameState)

-- Infinite lists of available IDs don't play nicely with derived Show
instance Show GameState where
    show g = "GameState {objects = " <> show (objects g) <> "}"

-- | Get the objects in the game
objects :: GameState -> ObjectGroup
objects = objs

objects' :: (ObjectGroup -> ObjectGroup) -> GameState -> GameState
objects' = objs'

-- | Fetch a specific object
object :: ID -> GameState -> GameObject
object i g = lookup i (objects g) <?> error ("Couldn't find object " <> show i)

-- | Update an object
object' :: (GameObject -> GameObject) -> ID -> GameState -> GameState
object' f i = objs' $ modify (Just . f) i >>> (<?> error ("Couldn't update object " <> show i))

-- | Get the ID of the player
player :: GameState -> ID
player = (<?> error "No player!") . head . keys . filter isPlayer . objects

-- | Transform the player
player' :: (GameObject -> GameObject) -> GameState -> GameState
player' f = object' f =<< player

-- | Add an object into the game
addObject :: GameObject -> GameState -> GameState
addObject obj s = (do
                    i <- head $ ids s
                    rest <- tail $ ids s
                    return $ objs' (insert i obj) $ ids' (\_-> rest) s
                  )
                <?> error "Ran out of ID's"

-- | Remove an object by ID
deleteObj :: ID -> GameState -> GameState
deleteObj i = ids' (i:)
          >>> objs' (delete i >>> (<?> error ("Object " <> show i <> " doesn't exist")))

-- | Game state with nothing in it
emptyState :: Bounds -> GameState
emptyState b = GameState b [0..] mempty
