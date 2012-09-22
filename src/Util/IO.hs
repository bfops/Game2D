-- | Functions for dealing with non-pure or non-lazy computations
module Util.IO ( IO
               , module Env
               , module Exit
               , undefined
               , void
               , forever
               , io
               , forceIO
               , runIO
               , run
               ) where

import Prelude (undefined)

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified System.IO as IO
import System.Environment as Env
import System.Exit as Exit

import Util.Prelewd

type IO = MaybeT IO.IO

io :: IO.IO a -> IO a
io = lift

-- | Force an IO computation to Haskell's basic IO type
forceIO :: IO a -> IO.IO a
forceIO = fmap fromJust . runMaybeT

-- | Run the IO action as a basic IO statement, having no return value
runIO :: IO a -> IO.IO ()
runIO = void . runMaybeT

run :: IO a -> IO ()
run = io . runIO
