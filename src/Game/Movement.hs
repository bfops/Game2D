{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
-- | Physical movement
module Game.Movement ( move
                     , Game.Movement.test
                     ) where

import Summit.Data.Map
import Summit.Data.Member
import Summit.Data.Set as Set hiding (size, insert)
import Summit.Impure
import Summit.Num.Nonfinite
import Summit.Prelewd hiding (Left, Right, filter)
import Summit.Subset.Num
import Summit.Test hiding (assert)

import Data.Tuple
import Text.Show
import Util.Range as Range

import Game.Physics
import Game.Vector hiding (normalize)
import Physics.Types
import Util.ID
import Util.Unit

foldl1' :: Foldable t => (a -> a -> a) -> t a -> a
foldl1' f t = foldl' (\a b -> f b <$> a <|> Just b) Nothing t
          <?> error "Empty foldl1'"

emptyRange :: Range a
emptyRange = Range.empty

-- | Choose between three options using an Ordering.
ordBy :: (a -> a -> Ordering)
      -> b    -- ^ LT option
      -> b    -- ^ EQ option
      -> b    -- ^ GT option
      -> a
      -> a
      -> b
ordBy f l e g x y = case f x y of
            LT -> l
            EQ -> e
            GT -> g

ord :: Ord a => b -> b -> b -> a -> a -> b
ord = ordBy compare

-- | A Range with some accompanying data
data TaggedRange a b = TaggedRange a (Range b)
    deriving (Show)

overlapTagged :: (Ord b, Monoid a) => TaggedRange a b -> TaggedRange a b -> TaggedRange a b
overlapTagged (TaggedRange x r1) (TaggedRange y r2) = let
                rng = r1 <> r2
                tag = iff (rng == emptyRange) mempty $ overlapNonempty r1 r2
            in TaggedRange tag rng
        where
            overlapNonempty = ord y (x <> y) x `on` nonfiniteToMaybe . startNonempty
            startNonempty r = start r <?> error "Empty overlap produced non-empty range"
            nonfiniteToMaybe n = mapInfinite (Just <$> n) Nothing

-- Shift one object through a vector towards another object
shift :: Position
      -> Physics
      -> Physics
      -> (Collisions, Nonfinite Scalar) -- ^ (Collisions, fraction of distance to be travelled)
                                        -- scalar is Infinite when no collisions take place
shift deltaP ph1 ph2 = let
                        -- Collisions individually in each dimension
                        collides1 = shift1 <$> dimensions <*> deltaP <*> posn ph1 <*> size ph1 <*> posn ph2 <*> size ph2
                        TaggedRange dims rng = normalize $ foldl1' overlapTagged collides1
                     in (dims, iff (end rng <= Just 0) Infinite $ start rng <?> Infinite)
    where
        -- Chop time ranges to [-Infinity, 1]
        normalize (TaggedRange t r) = TaggedRange t $ range Infinite 1 <> r

        -- Range of time during which the line (x1, w1) moving at shift towards overlaps (x2, w2)
        shift1 d v x1 w1 x2 w2 = TaggedRange (set [d])
                               $  pass1 v (x1 + fromPos w1) x2
                               <> pass1 (negate v) (x2 + fromPos w2) x1

        -- Range of time during which the point `x0`, moving at velocity `v`, is on the right side of `x`
        pass1 v x0 x = let t = point $ unitless $ (x - x0) / v
                       in case compare v 0 of
                        LT -> range Infinite (Finite t)
                        EQ -> iff (x0 <= x) emptyRange mempty
                        GT -> range (Finite t) Infinite

-- | Retrieve information about a potential object movement
move :: Position                            -- The movement to make (i.e. delta P)
     -> Physics                             -- Object to move
     -> Map ID Physics                      -- Objects to be checked for collisions
     -> (Position, Map ID Collisions)       -- The amount the object can be moved,
                                            -- and a map of collision object ID's to collision dimensions
move deltaP p = map (shift deltaP p)
            >>> \ps -> case foldl' min Infinite (snd <$> ps) of
                          Infinite -> (deltaP, mempty)
                          Finite moveTime -> ( (moveTime &*) <$> deltaP
                                             , fst <$> filter (snd >>> (== Finite moveTime)) ps
                                             )

test :: Test
test = $(testGroupGenerator)

prop_emptyMove :: (Physics, Position) -> Bool
prop_emptyMove (p, deltaP) = move deltaP p mempty == (deltaP, mempty)

prop_moveApart :: (Physics, Physics, Vector PhysicsValue, Direction) -> Bool
prop_moveApart (p1, p2, deltaP, d) = move shift' p1 (singleton 1 p2') == (shift', mempty)
    where
        (dim, flag) = d
        p2' = placeBehind p1 p2 dim flag
        shift' = directShift dim flag deltaP

prop_moveTogether :: (Physics, Physics, Vector PhysicsValue, Direction) -> Property
prop_moveTogether (p1, p2, deltaP, d) = component dim deltaP /= 0
                                     && all (< 1000) (abs $ deltaP <&> (/) <*> map (unitless.fromPos) (size p1))
                                     && all (< 1000) (abs $ deltaP <&> (/) <*> map (unitless.fromPos) (size p2))
                                     ==> let (s, cs) = move shift' p1 (singleton 1 p2')
                                         in s == pure 0
                                         && length cs == (1 :: Int)
                                         && (elem dim <$> lookup 1 cs) == Just True
    where
        (dim, flag) = d
        p2' = placeBehind p1 p2 dim flag
        shift' = directShift dim (not flag) deltaP

placeBehind :: Physics -> Physics -> Dimension -> Bool -> Physics
placeBehind p1 p2 dim pos = let delta = fromPos $ component dim $ size $ iff pos p1 p2
                            in p2 { posn = posn p1 <&> iff pos (+) (-) <*> singleV 0 dim delta }

directShift :: Dimension -> Bool -> Vector PhysicsValue -> Position
directShift dim neg = map point . component' dim (if' neg negate . abs)
