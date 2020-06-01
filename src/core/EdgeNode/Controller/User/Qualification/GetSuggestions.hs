{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Qualification.GetSuggestions (controller) where

import EdgeNode.Transport.User
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import Auth
import Katip
import KatipController
import Database.Transaction
import Control.Lens
import Data.Functor
import BuildInfo

controller :: UserId -> KatipController (Response QualificationSuggestions)
controller user_id = do
  runTelegram $location (user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- katipTransaction hasql $
    statement User.getSuggestions user_id
  $(logTM) DebugS $ logStr $ "suggestions: " <> show response
  runTelegram $location response $> fromEither response