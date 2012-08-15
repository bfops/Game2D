module Util.Range ( Range
                  , empty
                  , range
                  , start
                  , end
                  ) where

import Prelude ()
import Util.Prelewd hiding (empty)

import Data.Tuple
import Text.Show

import Util.Impure

newtype Range a = Range (Maybe (Indeterminate a, Indeterminate a))
    deriving (Eq, Show)

instance Ord a => Monoid (Range a) where
    mempty = Range $ Just (Infinite, Infinite)
    mappend (Range mr1) (Range mr2) = Range $ do r1 <- mr1
                                                 r2 <- mr2
                                                 overlapExtant r1 r2
        where
            -- Overlap nonempty ranges
            overlapExtant r1 r2 = assert (validRange r1 && validRange r2)
                                $ mcast validRange ((onBoth max `on` fst) r1 r2, (onBoth min `on` snd) r1 r2)

validRange :: Ord a => (Indeterminate a, Indeterminate a) -> Bool
validRange (t1, t2) = liftA2 (>=) t1 t2 /= pure True

-- | Apply a function across both parameters only if both exist;
-- otherwise default to the extant one
onBoth :: Alternative f => (a -> a -> a) -> f a -> f a -> f a
onBoth f x y = (f <$> x <*> y) <|> x <|> y

empty :: Range a
empty = Range Nothing

range :: Ord a => Indeterminate a -> Indeterminate a -> Range a
range x y = Range $ mcast validRange (x, y)

start :: Range a -> Maybe (Indeterminate a)
start (Range r) = fst <$> r

end :: Range a -> Maybe (Indeterminate a)
end (Range r) = snd <$> r
