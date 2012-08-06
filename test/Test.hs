module Main (main) where

import Test.Framework

import qualified Queue
import qualified Vector
import qualified InfNum
import qualified Fraction

main = defaultMain
        [ Queue.test
        , Vector.test
        , InfNum.test
        , Fraction.test
        ]
