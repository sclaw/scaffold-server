{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.User.Qualification.AddQualificationToTrajectory (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens
import qualified Data.Text as T
import Data.Bifunctor

controller :: [WithId (Id "qualification") AddQualification] -> UserId -> KatipController (Response [Id "user_qualification"])
controller qualifications user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let mkError _ = ("qualification is already at your skill's list" :: T.Text)
  fmap (fromEither . first mkError) $
    katipTransactionViolationError hasql $
      statement
      User.addQualification
      (user_id, qualifications)