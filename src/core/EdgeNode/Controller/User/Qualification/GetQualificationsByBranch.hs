{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.User.Qualification.GetQualificationsByBranch (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response

import KatipController
import Data.Aeson.WithField
import qualified Data.Text as T

controller :: Id "branch" -> KatipController (Response [WithId (Id "qualification") (OnlyField "value" T.Text)])
controller _ = pure $ Ok []