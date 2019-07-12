#!/usr/bin/env stack

import System.Directory 
import Data.Bool
import System.Process

main :: IO ()
main = 
  do 
   xs <- search "proto" 
   flip mapM_ xs $ \f -> do 
     let f' = tail $ dropWhile (/= '/') f
     callProcess "stack" 
      ["exec"
      , "compile-proto-file"
      , "--"
      , "--out"
      , "backend/proto/"
      ,  "--includeDir"
      , "proto/"
      , "--proto"
      , f']

search path =
  do
    entries <- listDirectory path
    r <- flip mapM entries $ \p -> do
      ok <- doesDirectoryExist $ path <> "/" <> p
      bool (return [path <> "/" <> p]) 
           (search (path <> "/" <> p)) ok
    return $ concat r