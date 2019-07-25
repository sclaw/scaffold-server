{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module RetrofitProto where

import qualified EdgeNode.Api.Http.Auth.Register as Auth
import qualified EdgeNode.Api.Http.Auth.SignIn as SignIn
import qualified EdgeNode.Api.Http.Auth.RefreshToken as RefreshToken

import TH.Instance
import GHC.Generics
import Data.Aeson
import Data.Swagger

deriveToSchemaAndJSONProtoEnum ''Auth.Error "Reg"
deriveToSchemaAndDefJSON ''ErrorReg
deriveToSchemaAndJSONProtoEnum ''SignIn.Error "SignIn"
deriveToSchemaAndDefJSON ''ErrorSignIn
deriveToSchemaAndJSONProtoEnum ''RefreshToken.Error "RefreshToken"
deriveToSchemaAndDefJSON ''ErrorRefreshToken

requestWrapper ''RefreshToken.Request
responseWrapper ''RefreshToken.Response
requestWrapper ''SignIn.Request
responseWrapper ''SignIn.Response
