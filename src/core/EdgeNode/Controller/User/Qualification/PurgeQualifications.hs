{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.User.Qualification.PurgeQualifications (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Database.Transaction
import Control.Lens
import Data.Int

controller :: Id "provider" -> [Id "qualification"] -> UserId -> KatipController (Response Int64)
controller provider_id xs user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap Ok $ katipTransaction hasql $ statement User.purgeQualifications (provider_id, user_id, xs)