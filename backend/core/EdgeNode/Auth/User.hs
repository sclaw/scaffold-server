{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module EdgeNode.Auth.User (JWTUser, jWTUserIdent, jWTUserRoles, jWTUserEmail) where

import EdgeNode.Api.User.JWTUser
import EdgeNode.Model.User.Entity ()
import EdgeNode.Model.Rbac.Entity ()

import Servant.Auth.Server
import TH.InstanceBuilder

instance FromJWT JWTUser where
  decodeJWT = undefined

instance ToJWT JWTUser where

deriveToSchemaAndJSON ''JWTUser