-- | Physical movement
module Game.Movement ( move
                     ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Text.Show

import Util.Fraction
import Util.Impure
import Util.Range

import Game.ObjectGroup
import Game.Physics
import Game.Vector

data TaggedRange a b = TaggedRange a (Range b)
    deriving Show

infToMaybe :: Indeterminate a -> Maybe a
infToMaybe Infinite = Nothing
infToMaybe (Finite x) = Just x

overlapTagged :: (Show a, Show b, Ord b, Monoid a) => TaggedRange a b -> TaggedRange a b -> TaggedRange a b
overlapTagged (TaggedRange x r1) (TaggedRange y r2) = let rng = r1 <> r2
                                                          tag = if rng == empty
                                                                then mempty
                                                                -- This quite rightly causes an error when it fails
                                                                else fromJust $ overlap <$> start r1 <*> start r2
                                                      in TaggedRange tag rng
        where
            overlap s1 s2 = case (compare `on` infToMaybe) s1 s2 of
                                LT -> y
                                EQ -> x <> y
                                GT -> x

shift :: DeltaP -> Physics -> Physics -> ([Dimension], Fraction Coord)
shift shiftV ph1 ph2 = let
                           -- Collisions individually in each dimension
                           collides1 = shift1 <$> dimensions <*> shiftV <*> posn ph1 <*> size ph1 <*> posn ph2 <*> size ph2
                           TaggedRange dims rng = foldr1 overlapTagged $ normalize <$> collides1
                     in maybe ([], 1) ((dims,) . recast) $ start rng
    where
        recast (Finite x) = x
        recast Infinite = error "recast parameter should be finite"

        -- Chop all time ranges to [0, 1]
        normalize (TaggedRange t r) = let r' = range (Finite 0) (Finite 1) <> r
                                      in if r' == empty
                                         then TaggedRange mempty empty
                                         else TaggedRange t r'

        -- Range of time during which the line (x1, w1) moving at shift towards overlaps (x2, w2)
        shift1 d v x1 w1 x2 w2 = TaggedRange [d] $ pass1 v (x1 + w1) x2 <> pass1 (negate v) (x2 + w2) x1

        -- Range of time during which the point x0, moving at v, is on the right side of x
        pass1 :: Distance -> Distance -> Distance -> Range (Fraction Time)
        pass1 v x0 x = let t = ((-) `on` realToFrac) x x0 / realToFrac v
                       in case compare v 0 of
                        LT -> range Infinite (Finite t)
                        EQ -> iff (x0 <= x) empty $ range Infinite Infinite
                        GT -> range (Finite t) Infinite

upd1 :: (a -> r) -> (a, b, c) -> (r, b, c)
upd1 f (a, b, c) = (f a, b, c)

-- | Retrieve information about a potential object movement
move :: DeltaP                      -- The movement to make (i.e. delta P)
     -> Physics                     -- Object to move
     -> [KeyPair ID Physics]        -- Rest of the objects
     -> (DeltaP, [ID], [Dimension]) -- The amount the object can be moved, the IDs it collides with, and how it collides
move shiftVector p = upd1 resolveT . foldr bumpList (Infinite, [], [])
    where
        resolveT Infinite = shiftVector
        resolveT (Finite t) = assert (t >= 0 && t <= 1) $ (realToFrac t *) <$> shiftVector

        bumpList iobj acc = keepEarlyColisns iobj acc $ shift shiftVector p $ val iobj

        keepEarlyColisns obj (c1, collides, dims) (ds, k) = let c2 = Finite k
                                                            in case compare c1 c2 of
                                                                LT -> (c1, collides, dims)
                                                                EQ -> (c2, id obj:collides, ds ++ dims)
                                                                GT -> (c2, [id obj], ds)
