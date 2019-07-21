{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module RetrofitProto 
      ( ErrorReg (..)
      , ErrorSignIn (..)
      ) where

import qualified EdgeNode.Api.Http.Auth.Register as Auth
import qualified EdgeNode.Api.Http.Auth.SignIn as SignIn

import TH.Instance 
  ( deriveToSchemaAndJSONProtoEnum 
  , deriveToSchemaAndDefJSON)
import GHC.Generics

deriveToSchemaAndJSONProtoEnum ''Auth.Error "Reg"
deriveToSchemaAndDefJSON ''ErrorReg
deriveToSchemaAndJSONProtoEnum ''SignIn.Error "SignIn"
deriveToSchemaAndDefJSON ''ErrorSignIn