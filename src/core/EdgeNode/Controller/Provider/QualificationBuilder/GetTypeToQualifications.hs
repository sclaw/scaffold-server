{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.QualificationBuilder.GetTypeToQualifications (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Id
import EdgeNode.Transport.Provider.Qualification

import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Katip
import TH.Proto
import Data.Aeson.WithField
import BuildInfo

controller
  :: EdgeNodeAcademicArea
  -> EdgeNodeCountry
  -> EdgeNodeQualificationDegree
  -> KatipController
     (Response
      [WithId (Id "qualification")
       DegreeTypeToQualification])
controller area country degree = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  runTelegram $location (area, country, degree)
  response <- katipTransaction hasql $
    statement
    Provider.getTypeToQualifications
    (area, country, degree)
  $(logTM) DebugS (logStr ("degrees: " ++ mkPretty mempty response))
  runTelegram $location response
  return $ Ok response