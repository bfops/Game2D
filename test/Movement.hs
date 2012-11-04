module Movement (test) where

import Prelewd hiding (Down)

import Impure

import Storage.Map
import Storage.Member
import Storage.Set
import Text.Show

import Game.Movement
import Game.Object
import Game.Physics
import Game.Vector

import Test.HUnit hiding (Test, test)
import Test.QuickCheck
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

test :: Test
test = $(testGroupGenerator)

data Direction = Up | Down | Left | Right
    deriving (Show, Eq, Enum, Bounded)

instance Arbitrary Direction where
    arbitrary = elements [ minBound .. maxBound ]

prop_emptyMove :: (Physics, Position) -> Bool
prop_emptyMove (p, shift) = move shift 0 p mempty == (shift, mempty)

prop_moveApart :: (Physics, Physics, Vector Double, Direction) -> Bool
prop_moveApart (p1, p2, shift, d) = move shift' 0 p1 (singleton 1 p2') == (shift', mempty)
    where
        p2' :: Physics
        shift' :: Position
        (p2', shift') = case d of
            Up -> setup Height False
            Down -> setup Height False
            Right -> setup Width True
            Left -> setup Width False

        setup dim flag = (placeBehind p1 p2 dim flag, dist <$> directShift dim flag shift)

prop_moveTogether :: (Physics, Physics, Vector Double, Direction) -> Property
prop_moveTogether (p1, p2, shift, d) = component dim shift /= 0
                                     ==> let (s, cs) = move shift' 0 p1 (singleton 1 p2')
                                         in s == pure 0
                                         && keys cs == [1]
                                         && (elem dim <$> lookup 1 cs) == Just True
    where
        p2' :: Physics
        shift' :: Position
        (p2', shift') = setup dim flag

        dim | elem d [Up, Down] = Height
            | elem d [Left, Right] = Width

        flag | elem d [Up, Right] = True
             | elem d [Down, Left] = False

        setup dim flag = (placeBehind p1 p2 dim flag, dist <$> directShift dim (not flag) shift)

placeBehind :: Physics -> Physics -> Dimension -> Bool -> Physics
placeBehind p1 p2 dim pos = let diff = abs $ component dim $ size $ iff pos p1 p2
                            in p2 { posn = posn p1 <&> iff pos (+) (-) <*> singleV 0 dim diff }

directShift :: Num a => Dimension -> Bool -> Vector a -> Vector a
directShift dim neg = component' dim $ if' neg (negate.) abs
