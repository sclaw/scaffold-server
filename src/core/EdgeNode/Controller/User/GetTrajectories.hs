{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.GetTrajectories (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.User
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Katip
import Database.Transaction
import Control.Lens
import Pretty

controller :: UserId -> KatipController (Response UserTrajectories)
controller user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  resp <- fmap Ok $ katipTransaction hasql $ statement User.getTrajectories user_id
  $(logTM) DebugS (logStr ("trajectories: " ++ mkPretty mempty resp))
  return resp