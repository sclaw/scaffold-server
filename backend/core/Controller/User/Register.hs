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

{-
password validation:
 Minimum eight characters, at least one letter and one number: ^(?=.*[A-Za-z])(?=.*\d)[A-Za-z\d]{8,}$

 Minimum eight characters, at least one letter,
 one number and one special character: ^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{8,}$

 Minimum eight characters, at least one uppercase letter,
 one lowercase letter and one number: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d]{8,}$

 Minimum eight characters, at least one uppercase letter,
 one lowercase letter, one number and one special character: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$
 
 Minimum eight and maximum 10 characters, at least one uppercase letter,
 one lowercase letter, one number and one special character: ^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,10}$

email validation: ^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]{2,}$
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

