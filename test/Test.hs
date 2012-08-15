module Main (main) where

import Test.Framework

import qualified Queue
import qualified Physics
import qualified InfNum
import qualified Fraction

main = defaultMain
        [ Queue.test
        , Physics.test
        , InfNum.test
        , Fraction.test
        ]
