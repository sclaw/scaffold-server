module EdgeNode.Controller.File.Delete (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.File as File

import KatipController
import Data.Aeson.Unit
import Control.Lens
import Database.Transaction

controller :: Id -> KatipController (Response Unit)
controller id = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (const (Ok Unit)) $ katipTransaction hasql $ statement File.delete id