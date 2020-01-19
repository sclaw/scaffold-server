module EdgeNode.Controller.File.Patch (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.File as File

import KatipController
import Data.Aeson.Unit
import Servant.Multipart.File
import Control.Lens
import Database.Transaction

controller :: Id -> File -> KatipController (Response Unit)
controller id _ = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  _ <- katipTransaction hasql $ statement File.getHash id
  undefined