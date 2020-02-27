{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.QualificationBuilder.GetAvailableBranches (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id

import KatipController
import Data.Aeson.WithField
import qualified Data.Text as T

controller :: Id "user" -> KatipController (Response [WithId (Id "branch") (OnlyField "title" T.Text)])
controller _ = undefined