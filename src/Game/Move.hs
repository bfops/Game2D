module Game.Move ( move
                 ) where

import Prelude ()
import Util.Prelewd hiding (id, empty)

import Text.Show

import Util.Fraction
import Util.Impure
import Util.Range

import Game.Physics
import Game.Types

data TaggedRange a b = TaggedRange a (Range b)
    deriving Show

infToMaybe :: InfNum a -> Maybe a
infToMaybe Infinite = Nothing
infToMaybe (Numb x) = Just x

taggedOverlap :: (Show a, Show b, Ord b, Monoid a) => TaggedRange a b -> TaggedRange a b -> TaggedRange a b
taggedOverlap (TaggedRange x r1) (TaggedRange y r2) = let rng = r1 <> r2
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

collideShift :: Position -> GameObject -> GameObject -> ([Dimension], Fraction Coord)
collideShift deltaP o1 o2 = let
                                shift = realToFrac <$> deltaP
                                p1 = realToFrac <$> posn (phys o1)
                                s1 = realToFrac <$> size (phys o1)
                                p2 = realToFrac <$> posn (phys o2)
                                s2 = realToFrac <$> size (phys o2)
                                -- Collisions individually in each dimension
                                collides1 = collideShift1 <$> dimensions <*> shift <*> p1 <*> s1 <*> p2 <*> s2
                                TaggedRange dims rng = foldr1 taggedOverlap $ normalize <$> collides1
                            in maybe ([], 1) ((dims,) . recast) $ start rng
    where
        recast (Numb x) = x
        recast Infinite = error "recast parameter should be finite"

        -- Chop all time ranges to [0, 1]
        normalize (TaggedRange t r) = let r' = range (Numb 0) (Numb 1) <> r
                                      in if r' == empty
                                         then TaggedRange mempty empty
                                         else TaggedRange t r'

        collideShift1 d shift x1 w1 x2 w2 = TaggedRange [d] $ pass1 shift (x1 + w1) x2 <> pass1 (negate shift) (x2 + w2) x1

        pass1 v x0 x = let t = (x - x0) / v
                       in case compare v 0 of
                        LT -> range Infinite (Numb t)
                        EQ -> iff (x0 <= x) empty $ range Infinite Infinite
                        GT -> range (Numb t) Infinite

upd1 :: (a -> r) -> (a, b, c) -> (r, b, c)
upd1 f (a, b, c) = (f a, b, c)

move :: Position                        -- The movement to make (i.e. delta P)
     -> GameObject                      -- Object to move
     -> ObjectGroup                     -- Rest of the objects
     -> (Position, [ID], [Dimension])   -- The amount the object can be moved, the IDs it collides with, and how it collides
move deltaP x = upd1 resolveT . foldr bumpList (Infinite, [], [])
    where
        resolveT Infinite = error "Something went infinitely wrong"
        resolveT (Numb t) = assert (t >= 0 && t <= 1) $ (realToFrac . (realToFrac t *)) <$> deltaP

        bumpList iobj acc = keepEarlyColisns iobj acc $ collideShift deltaP x $ val iobj

        keepEarlyColisns obj (c1, collides, dims) (ds, k) = let c2 = Numb k
                                                            in case compare c1 c2 of
                                                                LT -> (c1, collides, dims)
                                                                EQ -> (c2, id obj:collides, ds ++ dims)
                                                                GT -> (c2, [id obj], ds)
