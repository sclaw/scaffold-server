{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.Publish (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider

import KatipController
import Data.Aeson.Unit
import Database.Transaction
import Control.Lens

controller :: Id "user" -> KatipController (Response Unit)
controller uid = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  runTelegram uid
  fmap (const (Ok Unit)) $ katipTransaction hasql (statement Provider.publish uid)