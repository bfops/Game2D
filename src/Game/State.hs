{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
-- | Functions for dealing with game state
module Game.State ( GameState
                  , ObjectGroup
                  , Bounds
                  , bounds
                  , bounds'
                  , objects
                  , objects'
                  , object
                  , object'
                  , player
                  , addObject
                  , deleteObj
                  , emptyState
                  ) where

import Prelewd hiding (filter)

import Impure

import Data.Tuple
import Storage.List (head, tail)
import Storage.Map
import Template.MemberTransformer
import Text.Show

import Game.Object

-- | Uniquely identified group of GameObjects and updaters
type ObjectGroup = Map ID (GameObject, ObjectBehavior)

-- | Game state structure
data GameState = GameState { bounds :: Bounds
                           , ids    :: [ID]
                           , objs   :: ObjectGroup
                           }

$(memberTransformers ''GameState)

-- Only display the objects.
instance Show GameState where
    show g = "GameState {objects = " <> show (fst <$> objects g) <> "}"

-- | Get the objects in the game
objects :: GameState -> ObjectGroup
objects = objs

objects' :: (ObjectGroup -> ObjectGroup) -> GameState -> GameState
objects' = objs'

-- | Fetch a specific object
object :: ID -> GameState -> (GameObject, ObjectBehavior)
object i g = lookup i (objects g) <?> error ("Couldn't find object " <> show i)

-- | Update an object
object' :: ((GameObject, ObjectBehavior) -> (GameObject, ObjectBehavior))
        -> ID
        -> GameState
        -> GameState
object' f i = objects' $ modify (Just . f) i >>> (<?> error ("Couldn't update object " <> show i))

-- | Get the ID of the player
player :: GameState -> ID
player = (<?> error "No player!") . head . keys . filter (isPlayer . fst) . objects

-- | Add an object into the game
addObject :: ObjectBehavior -> GameObject -> GameState -> GameState
addObject b obj s = (do
                        i <- head $ ids s
                        rest <- tail $ ids s
                        return $ objects' (insert i (obj, b)) $ ids' (\_-> rest) s
                     )
                   <?> error "Ran out of IDs"

-- | Remove an object by ID
deleteObj :: ID -> GameState -> GameState
deleteObj i = ids' (i:)
          >>> objects' (delete i >>> (<?> error ("Object " <> show i <> " doesn't exist")))

-- | Game state with nothing in it
emptyState :: Bounds -> GameState
emptyState b = GameState b [0..] mempty
