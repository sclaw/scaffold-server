{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Auth.SignOut (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Transport.Error as Error
import qualified EdgeNode.Statement.Auth as Auth

import Auth
import KatipController
import Data.Aeson.Unit
import Data.Aeson.WithField
import qualified Data.Text as T
import Database.Transaction
import Data.Bifunctor
import Control.Lens
import Control.Concurrent.Lifted
import Control.Monad.IO.Class
import Control.Monad

controller :: UserId -> (OnlyField "hash" T.Text) -> KatipController (Response Unit)
controller user_id hash = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  telegram_service <- fmap (^.katipEnv.telegram) ask
  logger <- askLoggerIO
  void $ fork $ liftIO $ send telegram_service logger (T.pack (show (user_id, hash)))
  logout_e <- katipTransaction hasql $ statement Auth.logout (user_id, hash)
  let error = "credential not found"
  pure $ fromEither $ (first (const (Error.asError @T.Text error))) logout_e