{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller.User.Register (controller) where

import           Katip
import           KatipController
import           Network.WebSockets.Connection
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

controller :: Connection -> KatipController ()
controller conn = 
    do
      term <- (^.katipEnv.terminal) `fmap` ask
      let s = showColoredS term (Fg Red (Value "before action"))
      $(logTM) InfoS (logStr (mkPretty "debug info: " (s mempty)))
      liftIO $ conn `sendTextData` ("hello" :: Text)
 
      cm <- (^.katipEnv.ormDB) `fmap` ask
      _ :: Either SomeException [User] <- flip runTryDbConn cm $ do 
        $(logTM) InfoS (logStr ("inside action" :: String))
        select CondEmpty
        
      $(logTM) InfoS (logStr ("after action" :: String))