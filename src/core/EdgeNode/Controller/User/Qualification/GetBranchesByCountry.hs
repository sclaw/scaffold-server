{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.User.Qualification.GetBranchesByCountry (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Statement.User as User

import TH.Proto
import KatipController
import Data.Aeson.WithField
import qualified Data.Text as T
import Database.Transaction
import Control.Lens
import Data.Functor
import BuildInfo

controller :: EdgeNodeProviderCategory -> EdgeNodeQualificationDegree -> EdgeNodeCountry -> KatipController (Response [WithId (Id "branch") (OnlyField "title" T.Text)])
controller category degree_type country = do
  runTelegram $location (category, degree_type, country)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap Ok $ katipTransaction hasql $
    statement
    User.getBranchesByCountry
    (category, degree_type, country)
  runTelegram $location response $> response