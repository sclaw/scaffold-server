{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Trajectory.Add (controller) where

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
import Data.Traversable
import Data.Int
import qualified Data.Text as T

controller :: OnlyField "id" (Id "qualification") -> UserId -> KatipController (Response Unit)
controller qualification_id user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  resp <- katipTransactionViolationError hasql $ do
    deps_m <- statement User.getDepsQualifiationValues qualification_id
    compatilbity <- for deps_m $ \dep_xs -> do
       user_xs_m <- statement User.getUserQualificationValues user_id
       pure $ maybe 0.0 (calculateCompatibility dep_xs) user_xs_m
    statement User.addTrajectory (user_id, qualification_id, compatilbity)
  $(logTM) DebugS (logStr (show resp))
  let process (Right (Left e)) = Left e
      process (Right (Right _)) = Right Unit
      process (Left _) = Left User.AddTrajectoryErrorAlreadyAdded
  return $ (fromEither . process) resp

calculateCompatibility :: [(Int64, Int32, T.Text)] -> [(Int64, T.Text)] -> Double
calculateCompatibility _ _ = 100.0