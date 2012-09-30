-- | Adds type-checking units for calculations
module Util.Unit ( Unit
                 , unit
                 , scalar
                 , flexScalar
                 , strip
                 ) where

import Data.Map
import Test.QuickCheck
import Text.Show

import Util.Impure
import Util.Prelewd hiding (empty)

-- | `Unit t v` is a unit with a unit in `t`, and a value in `v`
data Unit t v = Unit { units :: Maybe (Map t Integer), val :: v }
    deriving Show

val' :: (v -> v') -> Unit t v -> Unit t v'
val' f x = x { val = f (val x) }

instance (Ord t, Eq v, Num v) => Eq (Unit t v) where
    (==) = (&&) .$ ((==) `on` val) $$ (compatible `on` units)

instance (Ord t, Num v, Ord v) => Ord (Unit t v) where
    compare = bool (error "Can't compare different units") .$ (compare `on` val) $$ (compatible `on` units)

instance (Show t, Ord t, Num v) => Num (Unit t v) where
    (+) = Unit .$ (combine `on` units) $$ ((+) `on` val)
        where combine x y = iff (compatible x y) (x <|> y) $ error $ "Can't add " <> show x <> " and " <> show y
    (*) = Unit .$ (liftA2 (unionWith (+)) `on` units) $$ ((*) `on` val)
    negate = val' negate
    abs = val' abs
    signum = val' signum
    fromInteger = flexScalar . fromInteger

instance (Show t, Ord t, Fractional v) => Fractional (Unit t v) where
    recip (Unit t v) = Unit (fmap negate <$> t) (recip v)
    fromRational = flexScalar . fromRational

instance (Ord t, Arbitrary t, Arbitrary v) => Arbitrary (Unit t v) where
    arbitrary = Unit . fmap fromList <$> arbitrary <*> arbitrary

compatible :: (Ord t, Eq v, Num v) => Maybe (Map t v) -> Maybe (Map t v) -> Bool
compatible x y = maybe True (all (== 0)) $ difference <$> x <*> y

infix 9 `unit`

-- | Construct a value with a unit
unit :: v -> t -> Unit t v
unit v t = Unit (Just $ singleton t 1) v

-- | A unitless value
scalar :: v -> Unit t v
scalar = Unit $ Just empty

-- | A value with inferred units
flexScalar :: v -> Unit t v
flexScalar = Unit Nothing

-- | Strip units from a value
strip :: Ord t => Map t Integer -> Unit t v -> Maybe v
strip ts = mcond <$> compatible (Just ts) . units <*> val
