module Util.Monad ( module M
                  , loop
                  , while
                  , doWhile
                  ) where

import Control.Monad as M

import Util.Bool

-- Repeat an action until it becoems false.
loop :: Monad m => m Bool -> m ()
loop a = a >>= bool (return ()) (loop a)

while :: Monad m => m Bool -> m a -> m ()
while p a = p >>= bool (return ()) (a >> while p a)

doWhile :: Monad m => m Bool -> m a -> m ()
doWhile p a = a >> while p a
