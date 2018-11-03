module Logger (init) where

import System.Directory
import Data.List (sortBy)
import System.FilePath.Posix 
       (takeBaseName, (<.>), (</>))
import Data.Maybe (listToMaybe)
import System.IO (IOMode(..), openFile, stdout)
import Data.Time.Calendar
import Data.Time.Clock
import Prelude hiding (init)
import System.Log.Logger.Extended
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (verboseStreamHandler)


init :: IO ()
init = 
    do
      fd <- getHdl
      let mkHdls h = 
           do let fmt = simpleLogFormatter "$time: [$loggername/$prio] $msg"
              flip setFormatter fmt <$> verboseStreamHandler h DEBUG
      handlers <- mapM mkHdls (fd : [stdout])
      updateGlobalLogger 
       rootLoggerName 
       (setHandlers handlers . setLevel DEBUG)
      infoMColored "app" "logging system initialized"      

getHdl =
    do
      createDirectoryIfMissing False "log"
      xs <- (listToMaybe . sortBy (flip compare)) `fmap` listDirectory "log"
      now <- getCurrentTime
      let curr = toGregorian $ utctDay now
      curr `mkHdl` xs
    where
        mkFileName (y, m, d) = show y ++ "-" ++ show m ++ "-" ++ show d
        mkHdl x@(year, month, day) Nothing =
            openFile ("log" </> mkFileName x <.> "log") AppendMode
        mkHdl x@(year, month, day) (Just file) =
            do
            let [yearS, monthS, dayS] = splitDay [] [] (takeBaseName file)
                analaze
                 | yearS == show year &&
                   monthS == show month &&
                   dayS == show day =
                    openFile
                    ("log" </> file)
                    AppendMode
                 | otherwise =
                    openFile
                    ("log" </> mkFileName x <.> "log")
                    AppendMode
            analaze
        splitDay y xs [] = reverse $ y : xs
        splitDay tmp ys (x:xs)
         | x == '-' = splitDay [] (tmp : ys) xs
         | otherwise = splitDay (tmp ++ [x]) ys xs