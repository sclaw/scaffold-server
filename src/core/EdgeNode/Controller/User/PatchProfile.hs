{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.PatchProfile (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User
import EdgeNode.Statement.User as User

import Auth
import KatipController
import Data.Aeson.WithField.Extended
import Data.Aeson.Unit
import Database.Transaction
import Control.Lens
import BuildInfo

controller :: OptField "image" (Id "image") Profile -> UserId -> KatipController (Response Unit)
controller patch user_id = do
  runTelegram $location (patch, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (const (Ok Unit)) $
    katipTransaction hasql $
      statement User.patchProfile (user_id, patch)