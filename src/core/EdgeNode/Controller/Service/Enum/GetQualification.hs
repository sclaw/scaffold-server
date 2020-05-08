{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Service.Enum.GetQualification (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Qualification

import TH.Proto
import KatipController

controller :: EdgeNodeLanguage -> KatipController (Response Enums)
controller _ = undefined