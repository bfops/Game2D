module Main (main) where

import Test.Framework

import qualified Queue
import qualified Physics
import qualified InfNum
import qualified Fraction
import qualified Movement

main = defaultMain
        [ Queue.test
        , Physics.test
        , InfNum.test
        , Fraction.test
        , Movement.test
        ]
