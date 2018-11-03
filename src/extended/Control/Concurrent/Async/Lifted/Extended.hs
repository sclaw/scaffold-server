{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.Async.Lifted.Extended
       (
           concurrentlyNThreadLifted
         , module Control.Concurrent.Async.Lifted
       ) where

import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import Control.Monad

concurrentlyNThreadLifted ::  MonadBaseControl IO m => [m ()] -> m ()
concurrentlyNThreadLifted []   = return ()
concurrentlyNThreadLifted [a] =
    async a >>= void . (wait :: (MonadBaseControl IO m => Async (StM m ()) -> m ()))
concurrentlyNThreadLifted (x:xs) = foldM glue void xs >>= flip ($) x
    where glue xs x = return (xs . concurrently_ x)
