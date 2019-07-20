{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module RetrofitProto 
      ( ErrorReg (..)
      , ErrorLogin (..)
      ) where

import qualified EdgeNode.Api.Http.Auth.Register as Auth
import qualified EdgeNode.Api.Http.Auth.Login as Login

import TH.Instance 
  ( deriveToSchemaAndJSONProtoEnum 
  , deriveToSchemaAndDefJSON)
import GHC.Generics

deriveToSchemaAndJSONProtoEnum ''Auth.Error "Reg"
deriveToSchemaAndDefJSON ''ErrorReg
deriveToSchemaAndJSONProtoEnum ''Login.Error "Login"
deriveToSchemaAndDefJSON ''ErrorLogin