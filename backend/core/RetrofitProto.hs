{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module RetrofitProto where

import qualified EdgeNode.Api.Http.Auth.Register as Reg
import qualified EdgeNode.Api.Http.Auth.SignIn as SignIn
import qualified EdgeNode.Api.Http.Auth.RefreshToken as RefreshToken

import TH.Generator
import GHC.Generics
import Data.Aeson
import Data.Swagger

deriveToSchemaAndJSONProtoEnum ''Reg.Error "Register"
deriveToSchemaAndDefJSON ''RegisterError
deriveToSchemaAndJSONProtoEnum ''SignIn.Error "SignIn"
deriveToSchemaAndDefJSON ''SignInError
deriveToSchemaAndJSONProtoEnum ''RefreshToken.Error "RefreshToken"
deriveToSchemaAndDefJSON ''RefreshTokenError

requestWrapper ''RefreshToken.Request
responseWrapper ''RefreshToken.Response
requestWrapper ''SignIn.Request
responseWrapper ''SignIn.Response
requestWrapper ''Reg.Request
responseWrapper ''Reg.Response