module Logger (getPath) where

import           Control.Exception     (ErrorCall (ErrorCall), throwIO)
import           Data.List             (sortBy)
import           Data.Maybe            (listToMaybe)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Prelude               hiding (init)
import           System.Directory
import           System.FilePath.Posix (FilePath, takeBaseName, (<.>), (</>))

getPath :: FilePath -> UTCTime -> IO FilePath
getPath path now =
    do
      createDirectoryIfMissing False path
      xs <- (listToMaybe . sortBy (flip compare)) `fmap` listDirectory path
      let curr = toGregorian $ utctDay now
      let m = curr `mkPath` xs
      let errMsg = ErrorCall $ "log file parse error: " ++ show xs
      maybe (throwIO errMsg) return m
    where
        mkFileName (y, m, d) = show y ++ "-" ++ show m ++ "-" ++ show d
        mkPath x Nothing = pure $ path </> mkFileName x <.> "log"
        mkPath x@(year, month, day) (Just file) =
          case splitDay [] [] (takeBaseName file) of
            [yearS, monthS, dayS] ->
              if yearS == show year &&
                 monthS == show month &&
                 dayS == show day
              then
                pure $ path </> file
              else
                pure $ path </> mkFileName x <.> "log"
            _ -> Nothing
        splitDay y xs [] = reverse $ y : xs
        splitDay tmp ys (x:xs)
          | x == '-' = splitDay [] (tmp : ys) xs
          | otherwise = splitDay (tmp ++ [x]) ys xs