{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.QualificationBuilder.Create (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id

import KatipController

controller :: KatipController (Response (Id "qualification"))
controller = undefined