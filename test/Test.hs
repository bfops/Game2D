module Main (main) where

import Test.Framework

import qualified Queue
import qualified Vector

main = defaultMain
        [ Queue.test
        , Vector.test
        ]
