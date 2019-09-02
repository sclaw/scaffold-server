{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified EdgeNode.Api.Http.User.GetTrajectories as GetTrajectories
import qualified EdgeNode.Api.Http.User.SaveTrajectory as SaveTrajectory
import EdgeNode.Lang
import EdgeNode.Category
import EdgeNode.Country

import TH.Mk
import GHC.Generics
import Data.Aeson
import Data.Swagger
import Control.Lens

mkSRGEqEnum ''Reg.Error "Register"
mkToSchemaAndDefJSON ''RegisterError
mkSRGEqEnum ''SignIn.Error "SignIn"
mkToSchemaAndDefJSON ''SignInError
mkSRGEqEnum ''RefreshToken.Error "RefreshToken"
mkToSchemaAndDefJSON ''RefreshTokenError
mkSRGEqEnum ''SaveTrajectory.Error "SaveTrajectory"
mkToSchemaAndDefJSON ''SaveTrajectoryError

mkRequestWrapper ''RefreshToken.Request
mkResponseWrapper ''RefreshToken.Response
mkRequestWrapper ''SignIn.Request
mkResponseWrapper ''SignIn.Response
mkRequestWrapper ''Reg.Request
mkResponseWrapper ''Reg.Response
mkRequestWrapper ''Countries.Request
mkResponseWrapper ''Countries.Response
mkRequestWrapper ''SaveQualifications.Request
mkResponseWrapper ''SaveQualifications.Response
mkResponseWrapper ''GetQualificationFullInfo.Response
mkResponseWrapper ''GetCategories.Response
mkResponseWrapper ''GetProviders.Response
mkResponseWrapper ''GetQualififcations.Response
mkRequestWrapper ''GetTrajectories.Request
mkResponseWrapper ''GetTrajectories.Response
mkRequestWrapper ''SaveTrajectory.Request
mkResponseWrapper ''SaveTrajectory.Response

mkEnumConvertor ''Language
mkEnumConvertor ''Type
mkEnumConvertor ''Country

mkSRGEqEnum ''Country "Wrapper"
mkEnumConvertor ''WrapperCountry
mkParamSchemaEnum ''WrapperCountry
mkFromHttpApiDataEnum ''WrapperCountry

mkSRGEqEnum ''Type "Wrapper"
mkEnumConvertor ''WrapperType
mkParamSchemaEnum ''WrapperType
mkFromHttpApiDataEnum ''WrapperType