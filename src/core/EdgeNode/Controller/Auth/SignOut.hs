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

controller :: UserId -> (OnlyField "hash" T.Text) -> KatipController (Response Unit)
controller user_id hash = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  logout_e <- katipTransaction hasql $ statement Auth.logout (user_id, hash)
  let error = "credential not found"
  pure $ fromEither $ (first (const (Error.asError @T.Text error))) logout_e