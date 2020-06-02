{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Qualification.MarkSuggestion (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.User
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User
import EdgeNode.Transport.Provider.Pool.Tags
import qualified EdgeNode.Controller.User.Trajectory.Add as Add

import Auth
import Katip
import KatipController
import Database.Transaction
import Control.Lens
import BuildInfo
import Data.Aeson.Unit
import Data.Coerce
import Proto3.Suite.Types
import Data.Int
import qualified Data.Text as T
import Data.Foldable
import Data.Aeson.WithField

controller :: Id "suggestion" -> QualificationSuggestion -> UserId -> KatipController (Response Unit)
controller _ suggestion _ |
  coerce @_ @(Either Int32 TrajectoryStatus)
  (qualificationSuggestionStatus suggestion) ==
  Right TrajectoryStatusNew = pure $ Error $ asError @T.Text "wrong status"
controller suggestion_id suggestion user_id = do
  runTelegram $location (suggestion_id, suggestion, user_id)
  $(logTM) DebugS $ logStr $ "mark suggestion: " <> show (suggestion_id, suggestion, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  qual_m <- katipTransaction hasql $ statement User.markSuggestion (suggestion_id, user_id, suggestion)
  for_ qual_m $ \ident -> Add.controller (coerce ident) user_id
  pure $ Ok Unit