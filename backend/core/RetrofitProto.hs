{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module RetrofitProto where

import qualified EdgeNode.Api.Http.Auth.Register as Auth
import qualified EdgeNode.Api.Http.Auth.SignIn as SignIn
import qualified EdgeNode.Api.Http.Auth.RefreshToken as RefreshToken

import TH.Instance 
  ( deriveToSchemaAndJSONProtoEnum 
  , deriveToSchemaAndDefJSON)
import GHC.Generics

deriveToSchemaAndJSONProtoEnum ''Auth.Error "Reg"
deriveToSchemaAndDefJSON ''ErrorReg
deriveToSchemaAndJSONProtoEnum ''SignIn.Error "SignIn"
deriveToSchemaAndDefJSON ''ErrorSignIn
deriveToSchemaAndJSONProtoEnum ''RefreshToken.Error "RefreshToken"
deriveToSchemaAndDefJSON ''ErrorRefreshToken