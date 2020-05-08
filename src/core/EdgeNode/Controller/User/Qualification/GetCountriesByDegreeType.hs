{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Qualification.GetCountriesByDegreeType (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries (EdgeNodeCountryCapture)
import EdgeNode.Statement.User as User

import TH.Proto
import KatipController
import Database.Transaction
import Control.Lens
import Data.Functor
import BuildInfo

controller :: EdgeNodeProviderCategory -> EdgeNodeQualificationDegree -> KatipController (Response [EdgeNodeCountryCapture])
controller category degree_type = do
  runTelegram $location (category, degree_type)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap Ok $ katipTransaction hasql $
    statement
    User.getCountriesByDegreeType
    (category, degree_type)
  runTelegram $location response $> response