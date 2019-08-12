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
import qualified EdgeNode.Service.Countries as Countries 
import qualified EdgeNode.Api.Http.User.SaveQualification as SaveQualification 
import qualified EdgeNode.Api.Http.User.GetQualificationFullInfo as GetQualificationFullInfo
import qualified EdgeNode.Api.Http.User.GetCategories as GetCategories 
import qualified EdgeNode.Api.Http.User.GetProvider as GetProvider 
import EdgeNode.Lang

import TH.Generator
import GHC.Generics
import Data.Aeson
import Data.Swagger
import Control.Lens

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
requestWrapper ''Countries.Request
responseWrapper ''Countries.Response
requestWrapper ''SaveQualification.Request
responseWrapper ''SaveQualification.Response
responseWrapper ''GetQualificationFullInfo.Response
responseWrapper ''GetCategories.Response
requestWrapper ''GetProvider.Request
responseWrapper ''GetProvider.Response

enumConvertor ''Language