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

writeTVar' :: T.TVar a -> a -> STM a
writeTVar' tv v = v <$ T.writeTVar tv v

modifyTVar :: T.TVar a -> (a -> a) -> STM a
modifyTVar tv f = f <$> T.readTVar tv >>= writeTVar' tv

modifyTVar' :: T.TVar a -> (a -> a) -> STM a
modifyTVar' tv = modifyTVar tv . ($!)
