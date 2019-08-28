{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

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
import qualified EdgeNode.Api.Http.User.GetTrajectories as GetTrajectories
import qualified EdgeNode.Api.Http.User.SaveTrajectory as SaveTrajectory
import EdgeNode.Lang
import EdgeNode.Category
import EdgeNode.Country

import TH.Generator
import GHC.Generics
import Data.Aeson
import Data.Swagger
import Control.Lens

deriveSRGEqEnum ''Reg.Error "Register"
deriveToSchemaAndDefJSON ''RegisterError
deriveSRGEqEnum ''SignIn.Error "SignIn"
deriveToSchemaAndDefJSON ''SignInError
deriveSRGEqEnum ''RefreshToken.Error "RefreshToken"
deriveToSchemaAndDefJSON ''RefreshTokenError
deriveSRGEqEnum ''SaveTrajectory.Error "SaveTrajectory"
deriveToSchemaAndDefJSON ''SaveTrajectoryError

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
responseWrapper ''GetProviders.Response
responseWrapper ''GetQualififcations.Response
requestWrapper ''Qualification.Request
responseWrapper ''Qualification.Response
requestWrapper ''GetTrajectories.Request
responseWrapper ''GetTrajectories.Response
requestWrapper ''SaveTrajectory.Request
responseWrapper ''SaveTrajectory.Response

enumConvertor ''Language
enumConvertor ''Type
enumConvertor ''Country

deriveSRGEqEnum ''Country "Wrapper"
enumConvertor ''WrapperCountry
mkParamSchemaEnum ''WrapperCountry
mkFromHttpApiDataEnum ''WrapperCountry

deriveSRGEqEnum ''Type "Wrapper"
enumConvertor ''WrapperType
mkParamSchemaEnum ''WrapperType
mkFromHttpApiDataEnum ''WrapperType