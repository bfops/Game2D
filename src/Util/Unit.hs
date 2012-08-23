module Util.Unit ( Unit
                 , unit
                 , scalar
                 , flexScalar
                 , strip
                 ) where

import Prelude ()
import Util.Prelewd hiding (empty)

import Data.Map

import Test.QuickCheck
import Text.Show

import Util.Impure

data Unit t v = Unit { units :: Maybe (Map t Integer), val :: v }
    deriving Show

val' :: (v -> v') -> Unit t v -> Unit t v'
val' f x = x { val = f (val x) }

instance (Ord t, Eq v, Num v) => Eq (Unit t v) where
    (==) = (&&) .$ ((==) `on` val) $$ (compatible `on` units)

instance (Ord t, Num v, Ord v) => Ord (Unit t v) where
    compare = compare `on` val

instance (Show t, Ord t, Num v) => Num (Unit t v) where
    (+) = Unit .$ (combine `on` units) $$ ((+) `on` val)
        where combine x y = iff (compatible x y) (x <|> y) $ error $ "Can't add " ++ show x ++ " and " ++ show y
    (*) = Unit .$ (onBoth (unionWith (+)) `on` units) $$ ((*) `on` val)
    negate = val' negate
    abs = val' abs
    signum = val' signum
    fromInteger = flexScalar . fromInteger

instance (Show t, Show v, Ord t, Fractional v) => Fractional (Unit t v) where
    recip (Unit t v) = Unit (fmap negate <$> t) (recip v)
    fromRational = flexScalar . fromRational

instance (Ord t, Arbitrary t, Arbitrary v) => Arbitrary (Unit t v) where
    arbitrary = Unit . fmap fromList <$> arbitrary <*> arbitrary

compatible :: (Ord t, Eq v, Num v) => Maybe (Map t v) -> Maybe (Map t v) -> Bool
compatible x y = maybe True (all (== 0)) $ symmetricDiff <$> x <*> y

symmetricDiff :: Ord t => Map t v -> Map t v -> Map t v
symmetricDiff x y = difference x y `union` difference y x

infix 9 `unit`

unit :: v -> t -> Unit t v
unit v t = Unit (Just $ singleton t 1) v

scalar :: v -> Unit t v
scalar = Unit $ Just empty

flexScalar :: v -> Unit t v
flexScalar = Unit Nothing

strip :: (Ord t, Show t) => Map t Integer -> Unit t v -> Maybe v
strip ts = mcond <$> compatible (Just ts) . units <*> val
