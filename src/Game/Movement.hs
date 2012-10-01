-- | Physical movement
module Game.Movement ( move
                     ) where

import Control.Arrow
import Text.Show

import Util.Impure
import Util.Map
import Util.Member
import Util.Prelewd hiding (empty)
import Util.Range as Range
import Util.Set

import Game.Object
import Game.Physics
import Game.Vector hiding (normalize)

data TaggedRange a b = TaggedRange a (Range b)
    deriving Show

indfToMaybe :: Indeterminate a -> Maybe a
indfToMaybe Infinite = Nothing
indfToMaybe (Finite x) = Just x

emptyRange :: Range a
emptyRange = Range.empty

overlapTagged :: (Ord b, Monoid a) => TaggedRange a b -> TaggedRange a b -> TaggedRange a b
overlapTagged (TaggedRange x r1) (TaggedRange y r2) = let rng = r1 <> r2
                                                          tag = if rng == emptyRange
                                                                then mempty
                                                                else overlapNonempty r1 r2
                                                      in TaggedRange tag rng
        where
            overlapNonempty = overlap `on` (error "Empty overlap produced non-empty range" <?>) . start
            overlap s1 s2 = case (compare `on` indfToMaybe) s1 s2 of
                                LT -> y
                                EQ -> x <> y
                                GT -> x

-- Shift one object through a vector towards another object
shift :: Position -> Physics -> Physics
      -> (Set Dimension, Scalar)        -- ^ (Collision dimensions, fraction of distance which can be travelled)
shift deltaP ph1 ph2 = let
                        -- Collisions individually in each dimension
                        collides1 = shift1 <$> dimensions <*> deltaP <*> posn ph1 <*> size ph1 <*> posn ph2 <*> size ph2
                        TaggedRange dims rng = normalize $ foldr1 overlapTagged $ collides1
                     in maybe (mempty, 1) ((dims,) . recast) $ start rng
    where
        recast (Finite x) = x
        recast Infinite = error "recast parameter should be finite"

        -- Chop all time ranges to [0, 1]
        normalize (TaggedRange t r) = let r' = range (Finite 0) (Finite 1) <> r
                                      in if r' == emptyRange
                                         then TaggedRange mempty emptyRange
                                         else TaggedRange t r'

        -- Range of time during which the line (x1, w1) moving at shift towards overlaps (x2, w2)
        shift1 d v x1 w1 x2 w2 = TaggedRange (set [d]) $ pass1 v (x1 + w1) x2 <> pass1 (negate v) (x2 + w2) x1

        -- Range of time during which the point x0, moving at v, is on the right side of x
        pass1 :: Speed -> Distance -> Distance -> Range Time
        pass1 v x0 x = let t = (x - x0) / v
                       in case compare v 0 of
                        LT -> range Infinite (Finite t)
                        EQ -> iff (x0 <= x) emptyRange $ range Infinite Infinite
                        GT -> range (Finite t) Infinite

-- | Retrieve information about a potential object movement
move :: Position                            -- The movement to make (i.e. delta P)
     -> ID
     -> Physics                             -- Object to move
     -> Map ID Physics                      -- All of the objects (this can include the object being moved)
     -> (Position, Map ID (Set Dimension))  -- The amount the object can be moved,
                                            -- and a map of collision object ID's to collision dimensions
move deltaP i p = first resolveT . foldrWithKey (\j obj -> if' (i /= j) $ earliestBump j obj) (Infinite, mempty)
    where
        resolveT Infinite = deltaP
        resolveT (Finite t) = assert (t >= 0 && t <= 1) $ (t *) <$> deltaP

        -- Take an object, some bump information and produce update earliest bump information
        earliestBump j q accum = keepEarlyColisns j accum $ shift deltaP p q

        keepEarlyColisns j (c1, collides) (ds, k) = assert (not $ elem j $ keys collides)
                                                  $ let c2 = Finite k
                                                    in case compare c1 c2 of
                                                        LT -> (c1, collides)
                                                        EQ -> (c2, insert j ds collides)
                                                        GT -> (c2, singleton j ds)
