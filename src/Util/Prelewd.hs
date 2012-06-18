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
                    , bool
                    , iff
                    , if'
                    , partition
                    , head
                    , last
                    , init
                    , tail
                    , length
                    , div
                    , divMod
                    , ifm
                    , mcond
                    , (!)
                    , (<&>)
                    , ($$)
                    , ($*)
                    , (.$)
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
                    ) where

import Prelude ( Int
               , Integer
               , Float
               , Double
               , Rational
               , Num
               , Real
               , Integral
               , Fractional
               , Floating
               , RealFrac
               , RealFloat
               , (+)
               , (-)
               , (*)
               , negate
               , abs
               , signum
               , fromInteger
               , toRational
               , quot
               , rem
               , mod
               , quotRem
               , toInteger
               , (/)
               , recip
               , fromRational
               , pi
               , exp
               , sqrt
               , log
               , (**)
               , logBase
               , sin
               , tan
               , cos
               , asin
               , atan
               , acos
               , sinh
               , tanh
               , cosh
               , asinh
               , atanh
               , acosh
               , properFraction
               , truncate
               , round
               , ceiling
               , floor
               , floatRadix
               , floatDigits
               , floatRange
               , decodeFloat
               , encodeFloat
               , exponent
               , significand
               , scaleFloat
               , isNaN
               , isInfinite
               , isDenormalized
               , isNegativeZero
               , isIEEE
               , atan2
               , subtract
               , even, odd
               , gcd
               , lcm
               , (^)
               , (^^)
               , fromIntegral
               , realToFrac
               )

import Control.Applicative hiding (Alternative (..), optional)
import Control.Monad hiding (mapM, mapM_, sequence, sequence_, msum, forM, forM_)
import Data.Bool
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Foldable
import Data.Function
import Data.List as List hiding (head, last, init, tail, partition, length, foldl, foldr)
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Traversable
import Data.Word

import Util.Impure
import Text.Show

-- | Process conditionals in the same form as `maybe` and `either` 
bool :: a    -- ^ Return if false
     -> a    -- ^ Return if true
     -> Bool
     -> a
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
div :: (Real a, Integral b) => a -> a -> b
div = div'

-- | `divMod a b = (div a b, mod a b)`
divMod :: (Real a, Integral b) => a -> a -> (b, a)
divMod = divMod'

-- | Conditionally create a monad
mcond :: MonadPlus m
      => Bool   -- ^ If this condition is false, mzero.
                -- Otherwise, return the value.
      -> a      -- ^ Value to make into a monad.
      -> m a
mcond = ifm .$ return

-- | Conditionally nullify a monad
ifm :: MonadPlus m
      => Bool   -- ^ If this condition is false, mzero.
                -- Otherwise, return the monad.
      -> m a    -- ^ Monad to filter
      -> m a
ifm = (>>) . guard

infixl 4 <&>, $$, $*
infixr 9 .$

-- | `(<$>)` with arguments interchanged
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- | `fmap` for functors-within-functors
($$) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
($$) = (<$>).(<$>)

-- | `(f $* g) x y = f x y $ g x y`
($*) :: (a -> b -> c -> d) -> (a -> b -> c) -> a -> b -> d
($*) f g x y = f x y $ g x y

-- | `(f .$ g) x y = f x (g y)`
(.$) :: (a -> b -> c) -> (r -> b) -> a -> r -> c
(.$) f g x = f x . g
