{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Trajectory.Remove (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Database.Transaction
import Control.Lens
import Data.Aeson.Unit
import BuildInfo

controller :: Id "trajectory" -> UserId -> KatipController (Response Unit)
controller trajectory_id user_id = do
  runTelegram $location (trajectory_id, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (const (Ok Unit)) $ katipTransaction hasql $ do
    statement User.apiCaller ($location, user_id)
    statement User.removeTrajectory (trajectory_id, user_id)