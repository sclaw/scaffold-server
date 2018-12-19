{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module BuildInfo (protoHash, location) where

import Data.String (fromString)
import Language.Haskell.TH (Exp(..), Lit(..))
import Language.Haskell.TH.Lib (ExpQ)
import Language.Haskell.TH.Syntax 
       (lift, loc_module, qLocation)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

{-# NOINLINE protoHashIO #-}
protoHashIO :: String
protoHashIO = unsafePerformIO mkHash
    where
        mkHash =   
         do
           let logArgs  = ["log", "-1", "--format=%h"]
           let git args = readProcess "git" args ""
           hash <- (head.lines) `fmap` git logArgs
           pure $ "web(commit):" ++ hash
       
protoHash :: ExpQ
protoHash = lift protoHashIO

location :: ExpQ
location = [| fromString $((LitE . StringL . loc_module) `fmap` qLocation) |]