{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.User.Qualification.GetQualificationsByBranch (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Data.Aeson.WithField
import qualified Data.Text as T
import Database.Transaction
import Control.Lens

controller :: Id "branch" -> UserId -> KatipController (Response [WithId (Id "qualification") (OnlyField "title" T.Text)])
controller ident user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap Ok $ katipTransaction hasql $ statement User.getQualificationsByBranch (user_id, ident)