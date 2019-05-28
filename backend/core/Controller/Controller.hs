{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}

module Controller.Controller (controller) where

import           Api
import           Katip
import           KatipHandler
import           Servant hiding (Application)
import           Network.WebSockets.Connection
import           Control.Monad.IO.Class
import           Data.ByteString.Char8 (pack)
import           Control.Monad.Reader.Class
import           Servant.API.WebSocket
import           Servant.Server.Generic
import           Servant.API.Generic
import           Control.Lens
import           Data.Text
import           Pretty
import           Servant.Server.Internal.ServantErr
import           Data.Monoid.Colorful
import           Data.Functor (($>)) 

controller :: ApplicationApi (AsServerT KatipHandler)
controller = 
  ApplicationApi 
  { root = $(logTM) InfoS (logStr (mkPretty "debug info: " "root")) $> "hello" 
  , home = toServant (HomeApi { getAllIntegers = getAllIntegersAction } :: HomeApi (AsServerT KatipHandler))  
  }

getAllIntegersAction :: Connection -> KatipHandler ()
getAllIntegersAction conn = 
    do
      term <- (^.katipEnv.terminal) `fmap` ask
      let s = showColoredS term (Fg Red (Value "hello"))
      $(logTM) InfoS (logStr (mkPretty "debug info: " (s mempty)))
      liftIO $ conn `sendTextData` ("hello" :: Text)