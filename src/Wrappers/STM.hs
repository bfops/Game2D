-- | STM wrapper, with some extra/modified functions
module Wrappers.STM ( module T
                    , module TC
                    , module TA
                    , atomically
                    , modifyTVar
                    , modifyTVar'
                    ) where

import Prelude (($!))

import qualified Control.Monad.STM as STM
import Control.Concurrent.STM.TVar as T hiding (modifyTVar, modifyTVar')
import Control.Concurrent.STM.TChan as TC
import Control.Concurrent.STM.TArray as TA

import Util.IO
import Util.Prelewd

-- | Perform an STM transaction atomically
atomically :: STM.STM a -> IO a
atomically = io . STM.atomically

-- | Write a value and return it
writeTVar' :: T.TVar a -> a -> STM.STM a
writeTVar' tv !v = v <$ writeTVar tv v

-- | Apply the function inside the TVar
modifyTVar :: T.TVar a -> (a -> a) -> STM.STM a
modifyTVar tv f = f <$> T.readTVar tv >>= writeTVar' tv

-- | Strict version of `modifyTVar`
modifyTVar' :: T.TVar a -> (a -> a) -> STM.STM a
modifyTVar' tv = modifyTVar tv . ($!)
