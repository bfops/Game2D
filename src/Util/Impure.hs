-- | Functions for dealing with non-pure or non-lazy computations
module Util.Impure ( module IO
                   , seq
                   , ($!)
                   , error
                   , undefined
                   ) where

import Prelude (seq, ($!), error, undefined)
import System.IO as IO
