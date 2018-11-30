{-# LANGUAGE TemplateHaskell #-}

module Logger (init, mkNextDayLogFile) where

import System.Directory
import Data.List (sortBy)
import System.FilePath.Posix
       (takeBaseName, (<.>), (</>))
import Data.Maybe (listToMaybe)
import System.IO (IOMode(..), openFile, stdout, FilePath)
import Data.Time.Calendar
import Data.Time.Clock
import Prelude hiding (init)
import System.Log.Logger.Extended
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (verboseStreamHandler)
import BuildInfo (protoHash)
import Control.Concurrent (threadDelay)


loc = "Logger."

init :: FilePath -> IO UTCTime
init path =
    do
      now <- getCurrentTime
      fd <- getHdl path now
      handlers <- mapM mkHdl (fd : [stdout])
      updateGlobalLogger
       rootLoggerName
       (setHandlers handlers . setLevel DEBUG)
      infoMColored
       (loc ++ "init")
       "logging system initialized"
      return now

mkHdl h =
    do
      let hash = "<" ++ $(protoHash) ++ ">"
          fmtStr = hash ++  " $time: [$loggername/$prio] $msg"
      flip setFormatter (simpleLogFormatter fmtStr) <$>
       verboseStreamHandler h DEBUG

mkFileName (y, m, d) = show y ++ "-" ++ show m ++ "-" ++ show d

getHdl path now =
    do
      createDirectoryIfMissing False path
      xs <- (listToMaybe . sortBy (flip compare)) `fmap` listDirectory path
      let curr = toGregorian $ utctDay now
      curr `mkHdl` xs
    where
        mkHdl x@(year, month, day) Nothing =
            openFile (path </> mkFileName x <.> "log") AppendMode
        mkHdl x@(year, month, day) (Just file) =
            do
            let [yearS, monthS, dayS] = splitDay [] [] (takeBaseName file)
                analize
                 | yearS == show year &&
                   monthS == show month &&
                   dayS == show day =
                    openFile (path </> file) AppendMode
                 | otherwise =
                    openFile (path </> mkFileName x <.> "log") AppendMode
            analize
        splitDay y xs [] = reverse $ y : xs
        splitDay tmp ys (x:xs)
         | x == '-' = splitDay [] (tmp : ys) xs
         | otherwise = splitDay (tmp ++ [x]) ys xs

mkNextDayLogFile :: FilePath -> UTCTime -> IO ()
mkNextDayLogFile path t =
    do
      threadDelay (300 * 10 ^ 6)
      now <- getCurrentTime
      let curr = toGregorian $ utctDay now
      let old = toGregorian $ utctDay t
      if curr /= old then
       do
         debugMColored
          (loc ++ "mkNextDayLogFile")
          "mkNew start.."
         fd <- mkNew path curr
         debugMColored
          (loc ++ "mkNextDayLogFile")
          "mkNew end"
         handlers <- mapM mkHdl (fd : [stdout])
         updateGlobalLogger
          rootLoggerName
          (setHandlers handlers . setLevel DEBUG)
         mkNextDayLogFile path now
      else
       mkNextDayLogFile path t

mkNew path new =
    do
      let file = path </> mkFileName new <.> "log"
      let infoMsg = "new log file created: " ++ file
      infoMColored (loc ++ "mkNextDayLogFile") infoMsg
      openFile file AppendMode