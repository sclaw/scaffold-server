module Control.Concurrent.Async.Extended
       (
          safeIO
        , safeIO_
        , raceNThread
        , concurrentlyNThread
        , module Control.Concurrent.Async
       ) where


import Control.Concurrent.Async
import Control.Exception
import Control.Monad


-- synonym for ordinary IO action that return nothing
type Action = IO ()

{-# INLINE safeIO #-}
safeIO :: IO a -> IO (Either SomeException a)
safeIO = (>>= waitCatch ) . async

-- like safeIO but discards result
{-# INLINE safeIO_ #-}
safeIO_ :: IO a -> IO ()
safeIO_ = void . safeIO

-- instead of defining permanently function for each io action
-- better be using raceNThread on list of io action
-- race f - IO b -> IO (Either a b)
-- raceNThread :: IO a -> ...  -> IO ()
-- raceNThread f1 .. fn =
--     void . race f' . ... race fn
-- kill related threads if exception rise
raceNThread :: [Action] -> IO ()
raceNThread [] = return ()
raceNThread [io] = async io >>= void . wait
raceNThread (x:xs) = foldM glue void xs >>= flip ($) x
    where glue xs x = return (xs . race_ x)

concurrentlyNThread :: [Action] -> IO ()
concurrentlyNThread []   = return ()
concurrentlyNThread [io] = async io >>= void . wait
concurrentlyNThread (x:xs) = foldM glue void xs >>= flip ($) x
    where glue xs x = return (xs . concurrently_ x)