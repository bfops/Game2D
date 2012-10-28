module Main (main) where

import Test.Framework

import qualified Vector
import qualified Movement
import qualified Range

main = defaultMain
        [ Vector.test
        , Movement.test
        , Range.test
        ]
