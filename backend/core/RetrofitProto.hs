{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module RetrofitProto (Error' (..)) where

import qualified EdgeNode.Api.Http.Auth.Register as Auth

import TH.Instance 
  ( deriveToSchemaAndJSONProtoEnum 
  , deriveToSchemaAndDefJSON)
import GHC.Generics

deriveToSchemaAndJSONProtoEnum ''Auth.Error "'"
deriveToSchemaAndDefJSON ''Error'