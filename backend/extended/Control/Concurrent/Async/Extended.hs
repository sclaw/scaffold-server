module Control.Concurrent.Async.Extended
       ( raceXs_
        , concurrentlyXs_
        , module Control.Concurrent.Async
       ) where

import Control.Concurrent.Async
import Control.Monad

raceXs_ :: [IO ()] -> IO ()
raceXs_ [] = return ()
raceXs_ [io] = async io >>= void . wait
raceXs_ (x:xs) = foldM glue void xs >>= flip ($) x
  where glue xs x = return (xs . race_ x)

concurrentlyXs_ :: [IO ()] -> IO ()
concurrentlyXs_ []   = return ()
concurrentlyXs_ [io] = async io >>= void . wait
concurrentlyXs_ (x:xs) = foldM glue void xs >>= flip ($) x
  where glue xs x = return (xs . concurrently_ x)