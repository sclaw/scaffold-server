{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Id

import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Katip
import TH.Proto

controller :: EdgeNodeAcademicArea -> Id "user" -> KatipController (Response [EdgeNodeCountry])
controller area uid = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask 
  resp <- katipTransaction hasql (statement Provider.getAreaToCountries (area, uid))
  $(logTM) DebugS (logStr ("branches: " ++ mkPretty mempty resp))
  return $ Ok resp