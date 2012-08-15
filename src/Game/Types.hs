module Game.Types ( GameObject (..)
                  , phys'
                  , isPlatform
                  , isBlock
                  , isPlayer
                  , KeyPair
                  , id
                  , val
                  , val'
                  , ID
                  , UniqueObject
                  , ObjectGroup
                  , GameState
                  , objects
                  , objects'
                  , object
                  , Input (..)
                  , Direction(..)
                  , addObject
                  , deleteObj
                  , emptyState
                  ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Text.Show

import Util.Impure

import Game.Physics

-- | An object in the game world
data GameObject
        = Block    { phys :: Physics }
        | Platform { phys :: Physics }
        | Player   { phys :: Physics }
    deriving Show

phys' :: (Physics -> Physics) -> GameObject -> GameObject
phys' f g = g { phys = f (phys g) }

isBlock :: GameObject -> Bool
isBlock (Block {}) = True
isBlock _ = False

isPlatform :: GameObject -> Bool
isPlatform (Platform {}) = True
isPlatform _ = False

isPlayer :: GameObject -> Bool
isPlayer (Player {}) = True
isPlayer _ = False

data KeyPair a b = KeyPair a b
    deriving (Show)

id :: KeyPair a b -> a
id (KeyPair a _) = a

val :: KeyPair a b -> b
val (KeyPair _ b) = b

val' :: (b -> b) -> KeyPair a b -> KeyPair a b
val' f (KeyPair a b) = KeyPair a (f b)

instance (Eq a) => Eq (KeyPair a b) where
    (==) = (==) `on` id

type ID = Integer
type UniqueObject = KeyPair ID GameObject
type ObjectGroup = [UniqueObject]

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

-- | Cardinal directions
data Direction = Up | Down | Left | Right
    deriving (Eq, Show)

-- | Input events understood by the game
data Input = Jump
           | Move Direction
    deriving (Eq, Show)

addObject :: GameObject -> GameState -> GameState
addObject obj (GameState { ids = (i:is), objects =                  objs })
             = GameState { ids =    is , objects = KeyPair i obj : objs }
addObject _ (GameState { ids = [] }) = error "Ran out of IDs"

deleteObj :: ID -> GameState -> Maybe GameState
deleteObj i (GameState { ids = is, objects = objs })
           = (\os -> GameState { ids = i:is, objects = os }) <$> deleteBy (id <&> (== i)) objs

emptyState :: GameState
emptyState = GameState { ids = [0..], objects = [] }
