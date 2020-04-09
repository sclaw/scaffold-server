module EdgeNode.Controller.User.Qualification.GetCountriesByDegreeType (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries (EdgeNodeCountryCapture)
import EdgeNode.Statement.User as User

import TH.Proto
import KatipController
import Database.Transaction
import Control.Lens

controller :: EdgeNodeProviderCategory -> EdgeNodeQualificationDegree -> KatipController (Response [EdgeNodeCountryCapture])
controller category degree_type = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap Ok $ katipTransaction hasql $
    statement
    User.getCountriesByDegreeType
    (category, degree_type)