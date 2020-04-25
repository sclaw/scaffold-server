{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Telegram (Service (..), mkService) where

import EdgeNode.Config (Telegram (..))

import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import Data.String.Conv
import Control.Monad
import Katip

newtype Service = Service { send :: (Severity -> LogStr -> IO ()) -> T.Text -> IO () }

mkService :: HTTP.Manager -> Telegram -> IO Service
mkService mgr Telegram {..} = do
  let url = telegramHost <> telegramBot <> "/sendMessage"
  req <- HTTP.parseRequest $ T.unpack url
  let send logger msg = do
        void $ logger DebugS (ls ("telegram req: " <> msg))
        resp <- flip HTTP.httpLbs mgr $
          HTTP.urlEncodedBody
          [ ("chat_id", toS ("@" <> telegramChat))
          , ("text", toS msg)
          ] req { HTTP.method = "POST" }
        void $ logger DebugS (ls ("telegram resp: " <> show resp))
  return Service {..}