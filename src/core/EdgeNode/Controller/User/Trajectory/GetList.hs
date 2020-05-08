{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Trajectory.GetList (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.User
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Katip
import Database.Transaction
import Control.Lens
import Pretty
import Data.Functor

controller :: UserId -> KatipController (Response UserTrajectories)
controller user_id = do
  runTelegram user_id
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap fromEither $ katipTransaction hasql $ statement User.getTrajectories user_id
  $(logTM) DebugS (logStr ("trajectories: " ++ mkPretty mempty response))
  runTelegram response $> response