{-# LANGUAGE CPP #-}
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
                    , module Data.Maybe
                    , module Data.Monoid
                    , module Data.Ord
                    , module Data.Traversable
                    , module Data.Word
                    , Indeterminate (..)
                    , apmap
                    , ordEq
                    , mconcat
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
                    , div
                    , divMod
                    , mcast
                    , mcond
                    , ifm
                    , (!)
                    , (<&>)
                    , (.)
                    , (.^)
                    , (.$)
                    , ($$)
                    , null
                    , reverse
                    , intersperse
                    , intercalate
                    , transpose
                    , subsequences
                    , permutations
                    , foldl1'
                    , scanl
                    , scanl1
                    , scanr
                    , scanr1
                    , iterate
                    , repeat
                    , replicate
                    , cycle
                    , unfoldr
                    , take
                    , drop
                    , splitAt
                    , takeWhile
                    , dropWhile
                    , dropWhileEnd
                    , span
                    , break
                    , stripPrefix
                    , group
                    , isPrefixOf
                    , isSuffixOf
                    , isInfixOf
                    , lookup
                    , filter
                    , zip
                    , zipWith
                    , (++)
                    , unzip
                    , sequence
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

import Control.Applicative hiding (optional, some, many)
import Control.Monad hiding (mapM, mapM_, sequence, sequence_, msum, forM, forM_, forever, void)
import Data.Bool
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Foldable hiding (concat, sequence_)
import Data.Function hiding (fix, (.))
import Data.List hiding (head, last, init, tail, partition, length, foldl, foldr, minimumBy, maximumBy, concat, deleteBy, foldr1, filter)
import Data.Int
import Data.Maybe
import Data.Monoid hiding (mconcat)
import Data.Ord
import Data.Traversable hiding (sequence)
import Data.Word

import Test.QuickCheck hiding (Fixed)
import Text.Show

import Util.Impure

#if __GLASGOW_HASKELL__ < 704
instance HasResolution a => Arbitrary (Fixed a) where
    arbitrary = realToFrac <$> (arbitrary :: Gen Double)
#endif

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
    mzero = empty
    mplus = (<|>)

instance Functor Indeterminate where
    fmap = apmap

instance Applicative Indeterminate where
    pure = return
    (<*>) = ap

instance Alternative Indeterminate where
    empty = Infinite
    Infinite <|> x = x
    x <|> _ = x

instance Arbitrary a => Arbitrary (Indeterminate a) where
    arbitrary = maybe Infinite Finite <$> arbitrary

-- | Default fmap inmplementation for Monads
apmap :: Applicative f => (a -> b) -> f a -> f b
apmap = (<*>) . pure

-- | Default == implementation for Ords
ordEq :: Ord a => a -> a -> Bool
ordEq x y = compare x y == EQ

-- | Generalized `mconcat`
mconcat :: (Foldable t, Monoid m) => t m -> m
mconcat = foldr (<>) mempty

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
last = Util.Prelewd.head . reverse

-- | All but the first element of a list
tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_:xs) = Just xs

-- | All but the last element of a list
init :: [a] -> Maybe [a]
init = Util.Prelewd.tail . reverse

-- | Find and remove the first occurance for which the supplied predicate is true
deleteBy :: (a -> Bool) -> [a] -> Maybe [a]
deleteBy _ [] = Nothing
deleteBy p (x:xs) = iff (p x) (Just xs) $ (x:) <$> deleteBy p xs

infixl 9 !

-- | `x ! i` is the `ith` element of `x`
(!) :: Foldable t => t a -> Integer -> a
(!) l n = case foldl (\v x -> v >>= go x) (Right n) l of
            Left x -> x
            Right i -> error $ "Foldable index too large by " ++ show (i + 1) ++ "."
    where
        go x 0 = Left x
        go _ k = Right (k - 1)

-- | Split a list into a pair of lists
partition :: (a -> Either b c) -> [a] -> ([b], [c])
partition f = partitionEithers . fmap f

-- | Length of a foldable structure
-- O(n)
length :: (Integral i, Foldable t) => t a -> i
length = foldr (const (+1)) 0

-- | Division with integral result
div :: (Real a, Integral b) => a -> a -> Indeterminate b
div x y = mcond (y /= 0) $ div' x y

-- | `divMod a b = (div a b, mod a b)`
divMod :: (Real a, Integral b) => a -> a -> Indeterminate (b, a)
divMod x y = mcond (y /= 0) $ divMod' x y

-- | Interpret something as a monad
mcast :: MonadPlus m
      => (a -> Bool)    -- ^ Casting function
      -> a              -- ^ Value to cast
      -> m a            -- ^ `return value` if cast succeeded, `mzero` otherwise
mcast = (mcond =<<)

-- | Conditionally create a monad
mcond :: MonadPlus m
      => Bool   -- ^ If this condition is false, mzero.
                -- Otherwise, return the value.
      -> a      -- ^ Value to make into a monad.
      -> m a
mcond = ifm .^ return

-- | Conditionally nullify a monad
ifm :: MonadPlus m
      => Bool   -- ^ If this condition is false, mzero.
                -- Otherwise, return the monad.
      -> m a    -- ^ Monad to filter
      -> m a
ifm = (>>) . guard

infixl 4 <&>

-- | `(<$>)` with arguments interchanged
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

infixl 9 ., .^
infixl 8 .$, $$

-- | `(f . g) x = f (g x)`
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

-- | Keep only elements which satisfy a predicate
filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter = mfilter

-- | Collect actions in a traversable structure
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = sequenceA

-- | Collect actions and discard results
sequence_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequence_ = sequenceA_

-- | Apply a function across both parameters only if both exist;
-- otherwise default to the extant one
onBoth :: Alternative f => (a -> a -> a) -> f a -> f a -> f a
onBoth f x y = (f <$> x <*> y) <|> x <|> y
