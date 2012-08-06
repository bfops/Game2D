-- | Functions for dealing with non-pure or non-lazy computations
module Util.Impure ( module T
                   , module E
                   , seq
                   , ($!)
                   , error
                   , withTrace
                   , force
                   ) where

import Prelude (seq, ($!), error)

import Control.Exception as E
import Debug.Trace as T hiding (putTraceMsg)
import Text.Show

-- | Traces and returns its argument
withTrace :: Show a => a -> a
withTrace x = traceShow x x

-- | `force x = seq x x`
force :: a -> a
force x = seq x x
