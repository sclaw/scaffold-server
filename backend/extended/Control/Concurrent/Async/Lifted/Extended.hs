{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.Async.Lifted.Extended
       (
           concurrentlyLiftedXs_
         , module Control.Concurrent.Async.Lifted
       ) where

import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import Control.Monad

concurrentlyLiftedXs_ ::  MonadBaseControl IO m => [m ()] -> m ()
concurrentlyLiftedXs_ [] = return ()
concurrentlyLiftedXs_ [a] = async a >>= void . (wait :: (MonadBaseControl IO m => Async (StM m ()) -> m ()))
concurrentlyLiftedXs_ (x:xs) = foldM glue void xs >>= flip ($) x
  where glue xs x = return (xs . concurrently_ x)
