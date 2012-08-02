module Util.Fraction ( Fraction (..)
                     ) where

import Prelude ()
import Util.Prelewd

import Data.Ratio
import Text.Show

data Fraction a = Frac a a
    deriving (Show)

instance Num a => Num (Fraction a) where
    (Frac n1 d1) + (Frac n2 d2) = Frac (n1*d2 + n2*d1) (d1 * d2)
    (Frac n1 d1) * (Frac n2 d2) = Frac (n1 * n2) (d1 * d2)
    negate (Frac a b) = Frac (negate a) b
    abs (Frac a b) = (Frac `on` abs) a b
    signum (Frac a b) = (Frac `on` signum) a b
    fromInteger i = Frac (fromInteger i) 1

instance Num a => Fractional (Fraction a) where
    recip (Frac a b) = Frac b a
    fromRational = (Frac `on` fromInteger) <$> numerator <*> denominator

instance (Num a, Eq a) => Eq (Fraction a) where
    (Frac n1 d1) == (Frac n2 d2) = (n1 * d2) == (n2 * d1)

flipOrd :: Ordering -> Ordering
flipOrd LT = GT
flipOrd GT = LT
flipOrd EQ = EQ

instance (Num a, Ord a) => Ord (Fraction a) where
    compare (Frac a1 a2) (Frac b1 b2) = if' (a2 < 0) flipOrd
                                      $ if' (b2 < 0) flipOrd
                                      $ compare (a1 * b2) (b1 * a2)
