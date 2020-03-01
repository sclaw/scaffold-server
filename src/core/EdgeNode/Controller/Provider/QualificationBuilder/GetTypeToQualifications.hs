{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.QualificationBuilder.GetTypeToQualifications (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Id
import EdgeNode.Transport.Qualification

import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Katip
import TH.Proto
import Data.Aeson.WithField

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
  resp <- katipTransaction hasql $ 
    statement 
    Provider.getTypeToQualifications
    (area, country, degree)
  $(logTM) DebugS (logStr ("degrees: " ++ mkPretty mempty resp))
  return $ Ok resp