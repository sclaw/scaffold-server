{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller.User.Register (controller) where

import           Api.User.Register.RegisterInfo
import           Api.User.Register.Response

import           Katip
import           KatipController
import           Network.WebSockets hiding (Response)
import           Control.Monad.IO.Class
import           Control.Lens ()
import           WebSocket ()
import           Text.ProtocolBuffers.Basic (defaultValue)
import           Control.Exception hiding (handle)
import           Control.Monad.Catch


{-'
      orm <- (^.katipEnv.ormDB) `fmap` ask
      _ :: Either SomeException [User] <- flip runTryDbConnOrm orm $ do 
        $(logTM) InfoS (logStr ("inside groundhog action" :: String))
        select CondEmpty
         
      io <-katipAddNamespace (Namespace ["raw"]) askLoggerIO

      raw <- (^.katipEnv.rawDB) `fmap` ask
      _ <- flip runTryDbConnRaw raw $ do
        liftIO $ io InfoS (logStr ("inside hasql  action" :: String))
        statement () (HS.Statement "" HE.unit HD.unit True)
-}


controller :: PendingConnection -> KatipController ()
controller pend = 
  do
    conn <- liftIO $ acceptRequest pend
    let header = pendingRequest pend
    katipAddNamespace (Namespace ["header"]) $ 
     $(logTM) InfoS (logStr (show header))
    handle err (run conn)
  where
    run conn = 
     do 
       info :: RegisterInfo <- liftIO $ evaluate =<< receiveData conn
       katipAddNamespace (Namespace ["request"]) $ 
        $(logTM) InfoS (logStr (show info))
       katipAddNamespace (Namespace ["response"]) $ do
        let resp = defaultValue :: Response 
        $(logTM) InfoS (logStr (show resp))
        liftIO $ conn `sendBinaryData` resp
    err :: SomeException -> KatipController ()  
    err e = katipAddNamespace (Namespace ["response"]) $ $(logTM) CriticalS (logStr (show e))

