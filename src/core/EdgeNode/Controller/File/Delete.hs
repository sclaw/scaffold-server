{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.File.Delete (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.File as File

import KatipController
import Data.Aeson.Unit
import Control.Lens
import Database.Transaction
import Data.Int
import Data.Coerce
import Control.Lens.Iso.Extended
import Data.Bool

controller :: Id "file" -> KatipController (Response Unit)
controller id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let notFound = "file {" <> show (coerce @(Id "file") @Int64 id)^.stext <> "} not found"
  runTelegram id
  isOk <- katipTransaction hasql $ statement File.delete id
  return $ bool (Error (asError notFound)) (Ok Unit) isOk