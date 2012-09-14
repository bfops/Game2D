-- | Functions and data structures for dealing with a world of objects
module Game.ObjectGroup ( ObjectGroup
                        , UniqueObject
                        , ID
                        , KeyPair (..)
                        , id'
                        , val'
                        ) where

import Util.Prelewd hiding (id, empty)

import Text.Show

import Game.Object

-- | Key-value pair; equality is evaluated based only on the keys
data KeyPair a b = KeyPair { id :: a
                           , val :: b
                           }
    deriving Show

-- | Transform id
id' :: (a -> r) -> KeyPair a b -> KeyPair r b
id' f (KeyPair a b) = KeyPair (f a) b

-- | Transform value
val' :: (b -> r) -> KeyPair a b -> KeyPair a r
val' f (KeyPair a b) = KeyPair a (f b)

instance (Eq a) => Eq (KeyPair a b) where
    (==) = (==) `on` id

instance Functor (KeyPair a) where
    fmap = val'

-- | Unique identifier
type ID = Integer
-- | Uniquely identified object
type UniqueObject = KeyPair ID GameObject
-- | World of objects
type ObjectGroup = [UniqueObject]
