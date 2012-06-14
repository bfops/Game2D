module Wrappers.STM ( module Control.Monad.STM
                    , module Control.Concurrent.STM.TMVar
                    , module Control.Concurrent.STM.TChan
                    , module Control.Concurrent.STM.TArray
                    , modifyTVar
                    , modifyTVar'
                    , T.TVar
                    , T.newTVar
                    , T.newTVarIO
                    , T.readTVar
                    , T.readTVarIO
                    , T.writeTVar
                    , T.swapTVar
                    , T.registerDelay
                    ) where

import Prelude (($!))
import Util.Prelewd

import Control.Monad.STM
import Control.Concurrent.STM.TVar as T hiding (modifyTVar, modifyTVar')
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TArray

-- | Apply the function inside the TVar
modifyTVar :: T.TVar a -> (a -> a) -> STM a
modifyTVar tv f = f <$> T.readTVar tv >>= writeTVar' tv

-- | Strict version of `modifyTVar`
modifyTVar' :: T.TVar a -> (a -> a) -> STM a
modifyTVar' tv = modifyTVar tv . ($!)
