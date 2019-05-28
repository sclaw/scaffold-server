{-# LANGUAGE TemplateHaskell #-}

module Controller.User.Register (controller) where

import           Katip
import           KatipHandler
import           Network.WebSockets.Connection
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Lens
import           Data.Text
import           Pretty
import           Data.Monoid.Colorful

controller :: Connection -> KatipHandler ()
controller conn = 
    do
      term <- (^.katipEnv.terminal) `fmap` ask
      let s = showColoredS term (Fg Red (Value "hello"))
      $(logTM) InfoS (logStr (mkPretty "debug info: " (s mempty)))
      liftIO $ conn `sendTextData` ("hello" :: Text)