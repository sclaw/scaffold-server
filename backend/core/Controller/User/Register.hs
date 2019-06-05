{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller.User.Register (controller) where

import           Api.User.Register.Request ()

import           Katip
import           KatipController
import           Network.WebSockets
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Lens
import           Data.Text
import           Pretty
import           Data.Monoid.Colorful
import           Database.Groundhog.Postgresql
import           Model.User.Entity
import           Database.Action
import           GHC.Exception.Type
import           Hasql.Session
import           Hasql.Statement as HS 
import           Hasql.Encoders as HE
import           Hasql.Decoders as HD
import           Katip.Monadic

controller :: PendingConnection -> KatipController ()
controller pend = 
    do
      conn <- liftIO $ acceptRequest pend
      let header = pendingRequest pend
      
      term <- (^.katipEnv.terminal) `fmap` ask
      let s = showColoredS term (Fg Red (Value (show header)))
      katipAddNamespace (Namespace ["header"]) $ 
       $(logTM) InfoS (logStr (mkPretty "request header: " (s mempty)))
      liftIO $ conn `sendTextData` ("hello" :: Text)
 
      orm <- (^.katipEnv.ormDB) `fmap` ask
      _ :: Either SomeException [User] <- flip runTryDbConnOrm orm $ do 
        $(logTM) InfoS (logStr ("inside groundhog action" :: String))
        select CondEmpty
         
      io <-katipAddNamespace (Namespace ["raw"]) askLoggerIO

      raw <- (^.katipEnv.rawDB) `fmap` ask
      _ <- flip runTryDbConnRaw raw $ do
        liftIO $ io InfoS (logStr ("inside hasql  action" :: String))
        statement () (HS.Statement "" HE.unit HD.unit True)

      $(logTM) InfoS (logStr ("contoller end" :: String))