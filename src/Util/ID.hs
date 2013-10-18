{-# LANGUAGE NoImplicitPrelude
           , GeneralizedNewtypeDeriving
           , DeriveDataTypeable
           , DeriveFoldable
           , DeriveFunctor
           , DeriveTraversable
           , TemplateHaskell
           #-}
module Util.ID ( ID
               , Named (..)
               , name
               , unname
               , call
               , call'
               , named
               , foldrWithID
               , mapWithID
               , alterNamed
               , mapMaybeWithID
               , mapMaybeNamed
               , filterWithID
               , filterNamed
               , unionNamed
               , updateNamed
               , nameAll
               , test
               ) where

import Summit.Impure
import Summit.Prelewd hiding (Traversable, foldr)
import Summit.Data.Map
import Summit.Data.Pair
import Summit.Data.Set (set, (\\))
import Summit.Test

import Data.List (foldr, deleteFirstsBy)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import Text.Show

deleteFirsts :: Eq a => [a] -> [a] -> [a]
deleteFirsts = deleteFirstsBy (==)

diffKeys :: Ord k => Map k v1 -> Map k v2 -> [k]
diffKeys m1 m2 = toList $ ((\\) `on` set) (keys m1) (keys m2)

newtype ID = ID Integer
  deriving (Show, Eq, Ord, Num, Integral, Real, Enum, Typeable, Arbitrary)

instance ResultEq ID

-- | Namedly identified group of objects.
data Named a = Named [ID] (Map ID a)
  deriving (Typeable, Functor, Foldable, Traversable)

instance Show a => Show (Named a) where
  show = show . named

instance Applicative Named where
  pure a = name a mempty
  Named fi f <*> Named _ a = Named (diffKeys f a <> fi) (intersectionWith ($) f a)

instance Monoid (Named a) where
  mempty = Named [0..] mempty
  mappend = unionNamed (\x _-> x)

instance Arbitrary a => Arbitrary (Named a) where
  arbitrary = foldr name mempty <$> arbitrary

instance Eq a => Eq (Named a) where
  (==) = (==) `on` named

instance ResultEq a => ResultEq (Named a)

ensure :: (ID -> Map ID a -> Maybe b) -> ID -> Map ID a -> b
ensure f i m = f i m <?> error ("Couldn't find " <> show i)

-- | Add an object to be named.
name :: a -> Named a -> Named a
name _ (Named [] _) = error "Ran out of IDs"
name a (Named (i:is) m) = Named is $ insert i a m

-- | Remove a specific ID.
unname :: ID -> Named a -> Named a
unname i (Named is m) = Named (i:is) $ delete i m <?> m

-- | Find a specific ID.
call :: ID -> Named a -> a
call i = ensure lookup i . named

-- | Modify a specific ID.
call' :: (a -> a) -> ID -> Named a -> Named a
call' f i (Named is m) = Named is $ ensure (modify $ Just . f) i m

-- | Get all named objects.
named :: Named a -> Map ID a
named (Named _ m) = m

mapWithID :: (ID -> a -> b) -> Named a -> Named b
mapWithID f (Named is m) = Named is $ mapWithKey f m

foldrWithID :: (ID -> a -> b -> b) -> b -> Named a -> b
foldrWithID f b = foldrWithKey f b . named

alterNamed :: (Maybe a -> a) -> ID -> Named a -> Named a
alterNamed f i (Named is m) = Named is $ alter (Just . f) i m

mapMaybeNamed :: (a -> Maybe b) -> Named a -> Named b
mapMaybeNamed f = mapMaybeWithID $ \_-> f

mapMaybeWithID :: (ID -> a -> Maybe b) -> Named a -> Named b
mapMaybeWithID f (Named is m) = let m' = mapMaybeWithKey f m
                                in Named (diffKeys m m' <> is) m'

filterWithID :: (ID -> a -> Bool) -> Named a -> Named a
filterWithID f = mapMaybeWithID $ cast . f

filterNamed :: (a -> Bool) -> Named a -> Named a
filterNamed = mapMaybeNamed . cast

unionNamed :: (a -> a -> a) -> Named a -> Named a -> Named a
unionNamed f (Named i1 m1) (Named _ m2) = Named (deleteFirsts (diffKeys m2 m1) i1)
                                                (unionWith f m1 m2)

updateNamed :: (a -> b -> b) -> Named a -> Named b -> Named b
updateNamed f a b = (f <$> a <*> b) <> b

test :: Test
test = $(testGroupGenerator)

nameAll :: Map ID a -> Named a
nameAll m = Named (deleteFirsts (keys m) [0..]) m

prop_union :: Pair (Map ID Integer) -> Result
prop_union ms = named (pair (<>) (nameAll <$> ms)) ==? pair (<>) ms
