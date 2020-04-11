{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.User.Qualification.PurgeQualifications (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Data.Aeson.Unit
import Database.Transaction
import Control.Lens

controller :: Id "provider" -> [Id "qualification"] -> UserId -> KatipController (Response Unit)
controller provider_id xs user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (const (Ok Unit)) $ katipTransaction hasql $ statement User.purgeQualifications (provider_id, user_id, xs)