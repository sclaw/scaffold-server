{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.User.Qualification.GetBranchesByCountry (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response

import TH.Proto
import KatipController
import Data.Aeson.WithField
import qualified Data.Text as T

controller :: EdgeNodeProviderCategory -> EdgeNodeQualificationDegree -> EdgeNodeCountry -> KatipController (Response [WithId (Id "branch") (OnlyField "title" T.Text)])
controller _ _ _ = pure $ Ok []