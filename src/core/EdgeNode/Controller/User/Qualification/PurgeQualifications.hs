{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Qualification.PurgeQualifications (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Database.Transaction
import Control.Lens
import Data.Int
import Data.Functor
import BuildInfo

controller :: Id "provider" -> [Id "qualification"] -> UserId -> KatipController (Response Int64)
controller provider_id xs user_id = do
  runTelegram $location (provider_id, xs, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap Ok $ katipTransaction hasql $ do
    statement User.apiCaller ($location, user_id)
    statement User.purgeQualifications (provider_id, user_id, xs)
  runTelegram $location response $> response