{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Mail (run) where

import EdgeNode.Mail
import EdgeNode.Statement.Mail as Mail

import KatipController
import Katip
import Control.Concurrent
import Database.Transaction
import Data.Pool
import Hasql.Connection
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Aeson
import Data.String.Conv
import qualified Network.Mail.Mime ()
import Mail.Smtp ()

run :: Pool Connection -> KatipLoggerIO -> IO ()
run pool logger = do
  logger InfoS $ ls @String "mailing service starts..."
  void $ transaction pool logger $ do
    xs <- statement Mail.getMails StatusNew
    liftIO $ logger InfoS $ ls $ "mail to be sent: " <> show xs
    for_ xs $ \(_, _, to_whom, msg) ->
      liftIO $ undefined $ Mail (toS to_whom) (toS (encode msg))
    statement Mail.setAsProcessed (StatusProcessed, mempty)
  threadDelay (30 * 10 ^ 6)
  run pool logger

-- test = do
--   let m = Network.Mail.Mime.simpleMail' (Network.Mail.Mime.Address Nothing "fclaw007@gmail.com") (Network.Mail.Mime.Address Nothing "info@edgenode.org") "saf" "asfs"
--   sendMailTlsDefPort "smtp.gmail.com" "fclaw007" "hosvxrxoiuauokae" m
