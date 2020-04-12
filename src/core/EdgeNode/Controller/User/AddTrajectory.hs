{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.AddTrajectory (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Katip
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens
import Data.Aeson.Unit

controller :: OnlyField "id" (Id "qualification") -> UserId -> KatipController (Response Unit)
controller qualification_id user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  resp <- katipTransactionViolationError hasql $ statement User.addTrajectory (user_id, qualification_id)
  $(logTM) DebugS (logStr (show resp))
  let process (Right (Left e)) = Left e
      process (Right (Right _)) = Right Unit
      process (Left _) = Left User.AddTrajectoryErrorAlreadyAdded
  return $ (fromEither . process) resp