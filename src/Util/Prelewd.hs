-- | Prelude replacement
-- Remember to import Prelude () if using this
module Util.Prelewd ( module Prelude
                    , module Control.Applicative
                    , module Control.Monad
                    , module Data.Bool
                    , module Data.Eq
                    , module Data.Foldable
                    , module Data.Function
                    , module Data.Int
                    , module Data.Monoid
                    , module Data.Ord
                    , module Data.Word
                    , Mappable (..)
                    , Sequential (..)
                    , Traversable (..)
                    , Indeterminate (..)
                    , Maybe (..)
                    , ordEq
                    , concat
                    , minBy
                    , maxBy
                    , bool
                    , iff
                    , if'
                    , partition
                    , head
                    , last
                    , init
                    , tail
                    , deleteBy
                    , length
                    , null
                    , div
                    , divMod
                    , maybe
                    , mapMaybe
                    , (<?>)
                    , cast
                    , mcond
                    , (!)
                    , ($>)
                    , (<&>)
                    , (.)
                    , (.^)
                    , (.$)
                    , ($$)
                    , List.reverse
                    , List.intersperse
                    , List.intercalate
                    , List.transpose
                    , List.subsequences
                    , List.permutations
                    , List.foldl1'
                    , List.scanl
                    , List.scanl1
                    , List.scanr
                    , List.scanr1
                    , List.iterate
                    , List.repeat
                    , replicate
                    , List.cycle
                    , List.unfoldr
                    , List.take
                    , List.drop
                    , List.splitAt
                    , List.takeWhile
                    , List.dropWhile
                    , List.dropWhileEnd
                    , List.span
                    , List.break
                    , List.stripPrefix
                    , List.group
                    , List.isPrefixOf
                    , List.isSuffixOf
                    , List.isInfixOf
                    , filter
                    , List.zip
                    , List.zipWith
                    , List.unzip
                    , sequence_
                    , onBoth
                    ) where

import Prelude ( Int
               , Integer
               , Float
               , Double
               , Rational
               , Enum (..)
               , Bounded (..)
               , Num (..)
               , Real (..)
               , Integral
               , Fractional (..)
               , Floating (..)
               , RealFrac (..)
               , RealFloat (..)
               , quot
               , rem
               , mod
               , quotRem
               , toInteger
               , subtract
               , even, odd
               , gcd
               , lcm
               , (^)
               , (^^)
               , fromIntegral
               , realToFrac
               , String
               )

import Control.Applicative (Applicative (..), Alternative (..), optional, (<$>), (<$), liftA, liftA2, liftA3)
import Control.Monad ( Functor (..), Monad (..), MonadPlus (..)
                     , (=<<), forever, void, guard, join, when, unless, ap, mfilter
                     )
import Data.Bool
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Foldable hiding (concat, sequence_, elem)
import Data.Function hiding (fix, (.))
import qualified Data.List as List
import Data.Int
import Data.Maybe hiding (mapMaybe)
import Data.Monoid
import Data.Ord
import qualified Data.Traversable as T
import qualified Data.Set as Set
import Data.Word

import Test.QuickCheck hiding (Fixed)
import Text.Show

import Util.Impure

-- | Multi-parameter Functor class
class Mappable f a b where
    map :: (a -> b) -> f a -> f b

-- | Multi-parameter class for `sequence`
class Applicative f => Sequential s f a where
    sequence :: s (f a) -> f (s a)

-- | Multi-parameter class for `traverse`
class (Foldable t, Mappable t a (f b), Sequential t f b) => Traversable t f a b where
    traverse :: (a -> f b) -> t a -> f (t b)
    traverse = sequence .$ map

instance Functor f => Mappable f a b where
    map = fmap

instance Applicative f => Sequential [] f a where
    sequence = T.sequenceA

instance Applicative f => Traversable [] f a b where
    traverse = T.traverse

instance Applicative f => Sequential Maybe f a where
    sequence = T.sequenceA

instance Applicative f => Traversable Maybe f a b where
    traverse = T.traverse

instance (Ord a, Ord b) => Mappable Set.Set a b where
    map = Set.map

instance (Applicative f, Ord a) => Sequential Set.Set f a where
    sequence = foldr (liftA2 Set.insert) (pure mempty)

-- | Objects with Infinity support
data Indeterminate a = Finite a
                     | Infinite
    deriving (Show, Eq)

instance Ord a => Ord (Indeterminate a) where
    compare (Finite _) Infinite = LT
    compare Infinite Infinite = EQ
    compare Infinite (Finite _) = GT
    compare (Finite x) (Finite y) = compare x y

instance Monad Indeterminate where
    return = Finite
    Infinite >>= _ = Infinite
    (Finite x) >>= f = f x

instance MonadPlus Indeterminate where
    mzero = mempty
    mplus = (<>)

instance Monoid (Indeterminate a) where
    mempty = Infinite
    mappend Infinite x = x
    mappend x _ = x

instance Functor Indeterminate where
    fmap = liftA

instance Applicative Indeterminate where
    pure = return
    (<*>) = ap

instance Alternative Indeterminate where
    empty = mempty
    (<|>) = (<>)

instance Arbitrary a => Arbitrary (Indeterminate a) where
    arbitrary = maybe Infinite Finite <$> arbitrary

-- | Default == implementation for Ords
ordEq :: Ord a => a -> a -> Bool
ordEq x y = compare x y == EQ

-- | Generalized `concat`
concat :: (Foldable t, Monoid m) => t m -> m
concat = foldr (<>) mempty

-- | `min` with user-supplied ordering
minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy f x y = minimumBy f [x, y]

-- | `max` with user-supplied ordering
maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy f x y = maximumBy f [x, y]

-- | Process conditionals in the same form as `maybe` and `either`
bool :: a -> a -> Bool -> a
bool f t b = iff b t f

-- | Function synonym for `if.. then.. else ..` 
iff :: Bool -> a -> a -> a
iff b t f = if b then t else f

-- | Conditionally apply a transformation function
if' :: Bool -> (a -> a) -> a -> a
if' b f = f >>= iff b 

-- | First element of a list
head :: [a] -> Maybe a
head = listToMaybe

-- | Last element of a finite list
last :: [a] -> Maybe a
last = Util.Prelewd.head . List.reverse

-- | All but the first element of a list
tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_:xs) = Just xs

-- | All but the last element of a list
init :: [a] -> Maybe [a]
init = Util.Prelewd.tail . List.reverse

-- | Find and remove the first occurance for which the supplied predicate is true
deleteBy :: (a -> Bool) -> [a] -> Maybe [a]
deleteBy _ [] = Nothing
deleteBy p (x:xs) = iff (p x) (Just xs) $ (x:) <$> deleteBy p xs

infixl 9 !

-- | `x ! i` is the `ith` element of `x`
(!) :: Foldable t => t a -> Integer -> a
(!) l n = case foldl (\v x -> v >>= go x) (Right n) l of
            Left x -> x
            Right i -> error $ "Foldable index too large by " <> show (i + 1) <> "."
    where
        go x 0 = Left x
        go _ k = Right (k - 1)

-- | Split a list into a pair of lists
partition :: (a -> Either b c) -> [a] -> ([b], [c])
partition f = partitionEithers . fmap f

-- | Length of a foldable structure
-- O(n)
length :: (Integral i, Foldable t) => t a -> i
length = foldl' (const . (+ 1)) 0

-- | Is this value empty?
null :: (Monoid a, Eq a) => a -> Bool
null x = x == mempty

-- | Division with integral result
div :: (Real a, Integral b) => a -> a -> Indeterminate b
div x y = mcond (y /= 0) $ div' x y

-- | `divMod a b = (div a b, mod a b)`
divMod :: (Real a, Integral b) => a -> a -> Indeterminate (b, a)
divMod x y = mcond (y /= 0) $ divMod' x y

-- | `map` with deletion
mapMaybe :: (Foldable m, Applicative m, Monoid (m b)) => (a -> Maybe b) -> m a -> m b
mapMaybe f = concat . fmap (maybe mempty pure . f)

infixl 4 <?>

-- | infix of `fromMaybe`
(<?>) :: a -> Maybe a -> a
(<?>) = fromMaybe

-- | Interpret something as a monad
cast :: MonadPlus m
     => (a -> Bool)    -- ^ Casting function
     -> a              -- ^ Value to cast
     -> m a            -- ^ `return value` if cast succeeded, `mzero` otherwise
cast = (mcond =<<)

-- | Conditionally create a monad
mcond :: MonadPlus m
      => Bool   -- ^ If this condition is false, mzero.
                -- Otherwise, return the value.
      -> a      -- ^ Value to make into a monad.
      -> m a
mcond = (>>) . guard .^ return

infixl 4 $>, <&>

-- | (<$), with arguments reversed
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

-- | `(<$>)` with arguments interchanged
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

infixl 9 ., .^
infixl 8 .$, $$

-- | `(f . g) x = f (g x)`
-- Redefined for fixity purposes
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) = fmap

-- | `(f .$ g) x y = f x (g y)`
(.^) :: (a -> b -> c) -> (r -> b) -> a -> r -> c
(.^) f g x = f x . g

-- | Composition across two arguments
(.$) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.$) = (.).(.)

-- | (f $$ g) x y = f x y $ g x y
($$) :: (x -> y -> a -> r) -> (x -> y -> a) -> x -> y -> r
($$) f g x = f x <*> g x

-- | Create several copies of an element
replicate :: Integral i => i -> a -> [a]
replicate = List.genericReplicate

-- | Keep only elements which satisfy a predicate
filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter = mfilter

-- | Collect actions and discard results
sequence_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequence_ = sequenceA_

-- | Null value for a type
-- | Apply a function across both parameters only if both exist;
-- otherwise default to the extant one
onBoth :: Alternative f => (a -> a -> a) -> f a -> f a -> f a
onBoth f x y = (f <$> x <*> y) <|> x <|> y
