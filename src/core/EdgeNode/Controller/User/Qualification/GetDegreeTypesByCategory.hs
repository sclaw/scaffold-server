{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Qualification.GetDegreeTypesByCategory (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User
import EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes (EdgeNodeQualificationDegreeCapture)

import TH.Proto
import KatipController
import Database.Transaction
import Control.Lens
import Data.Functor
import BuildInfo

controller :: EdgeNodeProviderCategory -> KatipController (Response [EdgeNodeQualificationDegreeCapture])
controller category = do
  runTelegram $location category
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap Ok $ katipTransaction hasql $ statement User.getDegreeTypesByCategory category
  runTelegram $location response $> response