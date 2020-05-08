{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.User.GetProfile (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens
import Data.Functor

controller :: UserId -> KatipController (Response (OptField "image" (Id "image") Profile))
controller user_id = do
  runTelegram user_id
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap Ok $ katipTransaction hasql $ statement User.getProfile user_id
  runTelegram response $> response