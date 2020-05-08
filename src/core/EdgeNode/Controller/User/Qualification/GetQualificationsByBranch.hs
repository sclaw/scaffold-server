{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.User.Qualification.GetQualificationsByBranch (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import TH.Proto
import Auth
import KatipController
import Data.Aeson.WithField
import qualified Data.Text as T
import Database.Transaction
import Control.Lens
import Data.Functor

controller :: Id "branch" -> EdgeNodeQualificationDegree -> UserId -> KatipController (Response [WithId (Id "qualification") (OnlyField "title" T.Text)])
controller ident degree_type user_id = do
  runTelegram (ident, degree_type, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap Ok $ katipTransaction hasql $ statement User.getQualificationsByBranch (user_id, ident, degree_type)
  runTelegram response $> response