{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.QualificationBuilder.Create (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Transport.Qualification

import KatipController

controller :: QualificationBuilder -> KatipController (Response (Id "qualification"))
controller _ = undefined