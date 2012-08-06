module Util.Range ( Range
                  , range
                  , start
                  , end
                  ) where

import Prelude ()
import Util.Prelewd

import Data.Tuple
import Text.Show

import Util.Impure

newtype Range a = Range (Maybe (InfNum a, InfNum a))
    deriving (Eq, Show)

instance Ord a => Monoid (Range a) where
    mempty = Range Nothing
    mappend (Range mr1) (Range mr2) = Range $ do r1 <- mr1
                                                 r2 <- mr2
                                                 overlapExtant r1 r2
        where
            -- Overlap nonempty ranges
            overlapExtant r1 r2 = assert (validRange r1 && validRange r2)
                                $ mcast validRange ((onBoth max `on` fst) r1 r2, (onBoth min `on` snd) r1 r2)

validRange :: Ord a => (InfNum a, InfNum a) -> Bool
validRange (t1, t2) = liftA2 (>=) t1 t2 /= pure True

-- | Apply a function across both parameters only if both exist;
-- otherwise default to the extant one
onBoth :: Alternative f => (a -> a -> a) -> f a -> f a -> f a
onBoth f x y = (f <$> x <*> y) <|> x <|> y

range :: Ord a => InfNum a -> InfNum a -> Range a
range x y = Range $ mcast validRange (x, y)

start :: Range a -> Maybe (InfNum a)
start (Range r) = fst <$> r

end :: Range a -> Maybe (InfNum a)
end (Range r) = snd <$> r
