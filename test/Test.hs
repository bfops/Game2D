module Main (main) where

import Test.Framework

import qualified Queue
import qualified Vector
import qualified Indeterminate
import qualified Fraction
import qualified Movement
import qualified Range

main = defaultMain
        [ Queue.test
        , Vector.test
        , Indeterminate.test
        , Fraction.test
        , Movement.test
        , Range.test
        ]
