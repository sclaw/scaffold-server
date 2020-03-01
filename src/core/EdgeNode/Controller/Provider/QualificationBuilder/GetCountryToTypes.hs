{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.Provider as Provider

import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Katip
import TH.Proto

controller 
  :: EdgeNodeAcademicArea 
  -> EdgeNodeCountry
  -> KatipController 
     (Response 
      [EdgeNodeQualificationDegree])
controller area country = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask 
  resp <- katipTransaction hasql $ 
    statement Provider.getCountryToTypes 
    (area, country)
  $(logTM) DebugS (logStr ("degrees: " ++ mkPretty mempty resp))
  return $ Ok resp