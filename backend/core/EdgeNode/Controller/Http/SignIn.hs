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

controller :: SignInRequest -> KatipController (Alternative ErrorSignIn SignInResponse)
controller req = 
  do
    let login = req^._Wrapped'.field @"requestLogin"
    let pass = req^._Wrapped'.field @"requestPassword"
    $(logTM) InfoS (logStr (login <> ", " <> pass))
    undefined