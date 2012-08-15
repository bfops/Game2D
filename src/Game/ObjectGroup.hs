module Game.ObjectGroup ( ObjectGroup
                        , UniqueObject
                        , ID
                        , KeyPair (..)
                        , val'
                        , id'
                        ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Text.Show

import Game.Object

-- | Key-value pair; equality is evaluated based only on the keys
data KeyPair a b = KeyPair { id :: a
                           , val :: b
                           }
    deriving Show

id' :: (a -> r) -> KeyPair a b -> KeyPair r b
id' f (KeyPair a b) = KeyPair (f a) b

val' :: (b -> r) -> KeyPair a b -> KeyPair a r
val' f (KeyPair a b) = KeyPair a (f b)

instance (Eq a) => Eq (KeyPair a b) where
    (==) = (==) `on` id

type ID = Integer
type UniqueObject = KeyPair ID GameObject
type ObjectGroup = [UniqueObject]
