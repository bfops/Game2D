module Main (main) where

import Test.Framework

import qualified Queue
import qualified Physics
import qualified Indeterminate
import qualified Fraction
import qualified Movement

main = defaultMain
        [ Queue.test
        , Physics.test
        , Indeterminate.test
        , Fraction.test
        , Movement.test
        ]
