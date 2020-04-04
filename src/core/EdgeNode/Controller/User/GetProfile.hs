{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.User.GetProfile (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Data.Aeson.WithField
import Database.Transaction
import Control.Lens

controller :: UserId -> KatipController (Response (WithField "image" (Maybe (Id "image")) Profile))
controller user_id = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap Ok $ katipTransaction hasql $ statement User.getProfile user_id