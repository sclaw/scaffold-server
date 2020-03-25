{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Provider.PatchQualification (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Transport.Qualification

import Auth
import KatipController
import Data.Aeson.Unit

controller :: Id "qualification" -> PatchQualification -> UserId -> KatipController (Response Unit)
controller _ _ _  = undefined