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
import qualified EdgeNode.Api.Http.User.SaveQualifications as SaveQualifications 
import qualified EdgeNode.Api.Http.User.GetQualificationFullInfo as GetQualificationFullInfo
import qualified EdgeNode.Api.Http.User.GetCategories as GetCategories 
import qualified EdgeNode.Api.Http.User.GetProviders as GetProviders
import qualified EdgeNode.Api.Http.User.GetQualififcations as GetQualififcations
import qualified EdgeNode.Api.Http.Search.Qualification as Qualification
import EdgeNode.Lang
import EdgeNode.Category
import EdgeNode.Country

import TH.Generator
import GHC.Generics
import Data.Aeson
import Data.Swagger
import Control.Lens

deriveSRGEnum ''Reg.Error "Register"
deriveToSchemaAndDefJSON ''RegisterError
deriveSRGEnum ''SignIn.Error "SignIn"
deriveToSchemaAndDefJSON ''SignInError
deriveSRGEnum ''RefreshToken.Error "RefreshToken"
deriveToSchemaAndDefJSON ''RefreshTokenError

requestWrapper ''RefreshToken.Request
responseWrapper ''RefreshToken.Response
requestWrapper ''SignIn.Request
responseWrapper ''SignIn.Response
requestWrapper ''Reg.Request
responseWrapper ''Reg.Response
requestWrapper ''Countries.Request
responseWrapper ''Countries.Response
requestWrapper ''SaveQualifications.Request
responseWrapper ''SaveQualifications.Response
responseWrapper ''GetQualificationFullInfo.Response
responseWrapper ''GetCategories.Response
requestWrapper ''GetProviders.Request
responseWrapper ''GetProviders.Response
requestWrapper ''GetQualififcations.Request
responseWrapper ''GetQualififcations.Response
requestWrapper ''Qualification.Request
responseWrapper ''Qualification.Response

enumConvertor ''Language
enumConvertor ''Type
enumConvertor ''Country