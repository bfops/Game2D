module Main (main) where

import IO

import Test.Framework

import qualified Vector
import qualified Movement
import qualified Range
import qualified Math

main :: SystemIO ()
main = defaultMain
        [ Vector.test
        , Movement.test
        , Range.test
        , Math.test
        ]
