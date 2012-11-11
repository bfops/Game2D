module Main (main) where

import IO

import Test.Framework

import qualified Vector
import qualified Movement
import qualified Range

main :: SystemIO ()
main = defaultMain
        [ Vector.test
        , Movement.test
        , Range.test
        ]
