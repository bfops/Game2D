-- | Functions for dealing with non-pure or non-lazy computations
module Util.Impure ( module T
                   , module E
                   , seq
                   , ($!)
                   , error
                   , withTrace
                   , force
                   , assert'
                   ) where

import Prelude

import Control.Exception as E
import Control.Monad.Instances ()
import Debug.Trace as T hiding (putTraceMsg)

-- | Traces and returns its argument
withTrace :: Show a => a -> a
withTrace x = traceShow x x

-- | `force x = seq x x`
force :: a -> a
force x = seq x x

-- | Assertation with a predicate
assert' :: (a -> Bool) -> a -> a
assert' = (=<<) assert
