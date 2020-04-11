{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.User.Qualification.GetQualificationList (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User
import EdgeNode.Transport.User

import Auth
import KatipController
import Database.Transaction
import Control.Lens

controller :: UserId -> KatipController (Response UserQualification)
controller user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap fromEither $ katipTransaction hasql $ statement User.getQualificationList user_id