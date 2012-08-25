-- | Physical movement
module Game.Movement ( move
                     ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Text.Show

import Util.Impure
import Util.Map
import Util.Range

import Game.ObjectGroup
import Game.Physics
import Game.Vector hiding (normalize)

data TaggedRange a b = TaggedRange a (Range b)
    deriving Show

infToMaybe :: Indeterminate a -> Maybe a
infToMaybe Infinite = Nothing
infToMaybe (Finite x) = Just x

emptyRange :: Range a
emptyRange = Util.Range.empty

overlapTagged :: (Show a, Show b, Ord b, Monoid a) => TaggedRange a b -> TaggedRange a b -> TaggedRange a b
overlapTagged (TaggedRange x r1) (TaggedRange y r2) = let rng = r1 <> r2
                                                          tag = if rng == emptyRange
                                                                then mempty
                                                                -- This quite rightly causes an error when it fails
                                                                else fromJust $ overlap <$> start r1 <*> start r2
                                                      in TaggedRange tag rng
        where
            overlap s1 s2 = case (compare `on` infToMaybe) s1 s2 of
                                LT -> y
                                EQ -> x <> y
                                GT -> x

shift :: Position -> Physics -> Physics -> ([Dimension], Scalar)
shift deltaP ph1 ph2 = let
                           -- Collisions individually in each dimension
                           collides1 = shift1 <$> dimensions <*> deltaP <*> posn ph1 <*> size ph1 <*> posn ph2 <*> size ph2
                           TaggedRange dims rng = foldr1 overlapTagged $ normalize <$> collides1
                     in maybe ([], 1) ((dims,) . recast) $ start rng
    where
        recast (Finite x) = x
        recast Infinite = error "recast parameter should be finite"

        -- Chop all time ranges to [0, 1]
        normalize (TaggedRange t r) = let r' = range (Finite 0) (Finite 1) <> r
                                      in if r' == emptyRange
                                         then TaggedRange mempty emptyRange
                                         else TaggedRange t r'

        -- Range of time during which the line (x1, w1) moving at shift towards overlaps (x2, w2)
        shift1 d v x1 w1 x2 w2 = TaggedRange [d] $ pass1 v (x1 + w1) x2 <> pass1 (negate v) (x2 + w2) x1

        -- Range of time during which the point x0, moving at v, is on the right side of x
        pass1 :: Speed -> Distance -> Distance -> Range Time
        pass1 v x0 x = let t = (x - x0) / v
                       in case compare v 0 of
                        LT -> range Infinite (Finite t)
                        EQ -> iff (x0 <= x) emptyRange $ range Infinite Infinite
                        GT -> range (Finite t) Infinite

upd1 :: (a -> r) -> (a, b) -> (r, b)
upd1 f (a, b) = (f a, b)

-- | Retrieve information about a potential object movement
move :: Position                        -- The movement to make (i.e. delta P)
     -> Physics                         -- Object to move
     -> [KeyPair ID Physics]            -- Rest of the objects
     -> (Position, Map ID [Dimension])  -- The amount the object can be moved, the IDs it collides with, and how it collides
move deltaP p = upd1 resolveT . foldr bumpList (Infinite, Util.Map.empty)
    where
        resolveT Infinite = deltaP
        resolveT (Finite t) = assert (t >= 0 && t <= 1) $ (t*) <$> deltaP

        bumpList iobj acc = keepEarlyColisns (id iobj) acc $ shift deltaP p $ val iobj

        keepEarlyColisns i (c1, collides) (ds, k) = assert (not $ member i collides)
                                                  $ let c2 = Finite k
                                                    in case compare c1 c2 of
                                                        LT -> (c1, collides)
                                                        EQ -> (c2, insert i ds collides)
                                                        GT -> (c2, singleton i ds)
