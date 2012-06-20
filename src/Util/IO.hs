-- | Functions for dealing with non-pure or non-lazy computations
module Util.IO ( module IO
               , seq
               , ($!)
               , undefined
               , void
               , forever
               ) where

import Prelude (seq, ($!), undefined)

import Control.Monad
import System.IO as IO
