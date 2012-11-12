module Movement (test) where

import Prelewd hiding (Down)

import Impure

import Data.Fixed
import Num.Positive
import Storage.Map
import Storage.Member
import Text.Show

import Game.Movement
import Game.Physics
import Game.Vector
import Util.Unit

import Test.QuickCheck
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

test :: Test
test = $(testGroupGenerator)

data Direction = Up | Down | Left | Right
    deriving (Show, Eq, Enum, Bounded, Ord)

instance Arbitrary Direction where
    arbitrary = elements [ minBound .. maxBound ]

dirToDim :: Direction -> Dimension
dirToDim d = lookup d (fromList
           $ [ (Left, Width), (Right, Width)
             , (Down, Height), (Up, Height)
             ])
           <?> error "Couldn't map direction to dimension"

sign :: Direction -> Bool
sign d = lookup d (fromList
       $ [ (Left, False), (Right, True)
         , (Down, False), (   Up, True)
         ])
     <?> error "Couldn't map direction to sign"

prop_emptyMove :: (Physics, Position) -> Bool
prop_emptyMove (p, shift) = move shift 0 p mempty == (shift, mempty)

prop_moveApart :: (Physics, Physics, Vector Milli, Direction) -> Bool
prop_moveApart (p1, p2, shift, d) = move shift' 0 p1 (singleton 1 p2') == (shift', mempty)
    where
        p2' = placeBehind p1 p2 dim flag
        shift' = directShift dim flag shift

        dim = dirToDim d
        flag = sign d

prop_moveTogether :: (Physics, Physics, Vector Milli, Direction) -> Property
prop_moveTogether (p1, p2, shift, d) = component dim shift /= 0
                                     && all (< 1000) (abs $ shift <&> (/) <*> map (unitless.num) (size p1))
                                     && all (< 1000) (abs $ shift <&> (/) <*> map (unitless.num) (size p2))
                                     ==> let (s, cs) = move shift' 0 p1 (singleton 1 p2')
                                         in s == pure 0
                                         && keys cs == [1]
                                         && (elem dim <$> lookup 1 cs) == Just True
    where
        p2' = placeBehind p1 p2 dim flag
        shift' = directShift dim (not flag) shift

        dim = dirToDim d
        flag = sign d

placeBehind :: Physics -> Physics -> Dimension -> Bool -> Physics
placeBehind p1 p2 dim pos = let diff = num $ component dim $ size $ iff pos p1 p2
                            in p2 { posn = posn p1 <&> iff pos (+) (-) <*> singleV 0 dim diff }

directShift :: Dimension -> Bool -> Vector Milli -> Position
directShift dim neg = map Unit . component' dim (if' neg negate . abs)
