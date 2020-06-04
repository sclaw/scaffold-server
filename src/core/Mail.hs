{-# LANGUAGE OverloadedStrings #-}

module Mail (runMailing) where

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

runMailing :: Pool Connection -> KatipLoggerIO -> IO ()
runMailing pool logger = do
  logger InfoS $ ls ("mailing service starts..." :: String)
  void $ transaction pool logger $ do
      xs <- statement Mail.getMails StatusNew
      liftIO $ logger InfoS $ ls $ "mail to be sent: " <> show xs
      statement Mail.setAsProcessed (StatusProcessed, mempty)
  threadDelay (30 * 10 ^ 6)
  runMailing pool logger