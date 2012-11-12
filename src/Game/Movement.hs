-- | Physical movement
module Game.Movement ( move
                     ) where

import Prelewd

import Impure

import Num.Indeterminate
import Num.Positive
import Storage.Map
import Storage.Member
import Storage.Set
import Text.Show
import Util.Range as Range

import Game.Object
import Game.Physics
import Game.Vector hiding (normalize)
import Util.Unit

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
            overlapNonempty = overlap `on` (<?> error "Empty overlap produced non-empty range") . start
            overlap s1 s2 = case (compare `on` indfToMaybe) s1 s2 of
                                LT -> y
                                EQ -> x <> y
                                GT -> x

-- Shift one object through a vector towards another object
shift :: Position -> Physics -> Physics
      -> (Set Dimension, Indeterminate Scalar)  -- ^ (Collision dimensions, fraction of distance to be travelled)
                                                -- scalar is Infinite when no collisions take place
shift deltaP ph1 ph2 = let
                        -- Collisions individually in each dimension
                        collides1 = shift1 <$> dimensions <*> deltaP <*> posn ph1 <*> size ph1 <*> posn ph2 <*> size ph2
                        TaggedRange dims rng = normalize $ foldr1 overlapTagged $ collides1
                     in (dims, iff (end rng <= Just 0) Infinite $ start rng <?> Infinite)
    where
        -- Chop time ranges to [-Infinity, 1]
        normalize (TaggedRange t r) = TaggedRange t $ range Infinite 1 <> r

        -- Range of time during which the line (x1, w1) moving at shift towards overlaps (x2, w2)
        shift1 d v x1 w1 x2 w2 = TaggedRange (set [d]) $ pass1 v (x1 + num w1) x2 <> pass1 (negate v) (x2 + num w2) x1

        -- Range of time during which the point `x0`, moving at velocity `v`, is on the right side of `x`
        pass1 v x0 x = let t = Unit $ unitless $ (x - x0) / v
                       in case compare v 0 of
                        LT -> range Infinite (Finite t)
                        EQ -> iff (x0 <= x) emptyRange mempty
                        GT -> range (Finite t) Infinite

-- | Retrieve information about a potential object movement
move :: Position                            -- The movement to make (i.e. delta P)
     -> ID
     -> Physics                             -- Object to move
     -> Map ID Physics                      -- All of the objects (this can include the object being moved)
     -> (Position, Map ID (Set Dimension))  -- The amount the object can be moved,
                                            -- and a map of collision object ID's to collision dimensions
move deltaP id p = resolveT . foldrWithKey (\i -> if' (id /= i) . earliestBump i) (Infinite, mempty)
    where
        resolveT (Infinite, _) = (deltaP, mempty)
        resolveT (Finite t, s) = ((t &*) <$> deltaP, s)

        -- Take an object, some bump information and produce update earliest bump information
        earliestBump j q accum = keepEarlyColisns j accum $ shift deltaP p q

        keepEarlyColisns j (c1, collides) (ds, c2) = assert (not $ elem j $ keys collides)
                                                   $ case compare c1 c2 of
                                                        LT -> (c1, collides)
                                                        EQ -> (c2, insert j ds collides)
                                                        GT -> (c2, singleton j ds)
