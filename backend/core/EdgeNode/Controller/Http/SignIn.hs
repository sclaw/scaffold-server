{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Http.SignIn (controller) where

import EdgeNode.Api.Http.Auth.SignIn ()

import RetrofitProto
import ReliefJsonData
import KatipController
import Control.Lens (_Wrapped')
import Katip
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product

controller :: SignInRequest -> KatipController (Alternative SignInError SignInResponse)
controller req = 
  do
    let email = req^._Wrapped'.field @"requestEmail"
    let pass = req^._Wrapped'.field @"requestPassword"
    $(logTM) InfoS (logStr (email <> ", " <> pass))
    undefined