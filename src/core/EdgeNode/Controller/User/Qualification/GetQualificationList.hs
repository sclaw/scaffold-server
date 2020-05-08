{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Qualification.GetQualificationList (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User
import EdgeNode.Transport.User

import Auth
import KatipController
import Database.Transaction
import Control.Lens
import Data.Functor
import BuildInfo

controller :: UserId -> KatipController (Response UserQualification)
controller user_id = do
  runTelegram $location user_id
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap fromEither $ katipTransaction hasql $ statement User.getQualificationList user_id
  runTelegram $location response $> response