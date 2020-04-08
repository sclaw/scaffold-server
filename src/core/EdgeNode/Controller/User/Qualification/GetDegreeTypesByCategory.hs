module EdgeNode.Controller.User.Qualification.GetDegreeTypesByCategory (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import TH.Proto
import KatipController
import Database.Transaction
import Control.Lens

controller :: EdgeNodeProviderCategory -> KatipController (Response [EdgeNodeQualificationDegree])
controller category = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap Ok $ katipTransaction hasql $ statement User.getDegreeTypesByCategory category