{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EdgeNode.Controller.Application (application) where

import EdgeNode.Api
-- controllers
import qualified EdgeNode.Controller.Http.Registration as Auth.Registration
import qualified EdgeNode.Controller.Http.SignIn as Auth.SignIn
import qualified EdgeNode.Controller.Http.LoadProfile as User.LoadProfile 
import qualified EdgeNode.Controller.Http.PatchProfile as User.PatchProfile
import qualified EdgeNode.Controller.Http.LoadCountries as Service.LoadCountries

import Katip
import KatipController
import Servant.Server.Generic
import Servant.API.Generic

application :: ApplicationApi (AsServerT KatipController)
application = ApplicationApi { applicationApiHttp = toServant httpApi }

httpApi :: HttpApi (AsServerT KatipController)
httpApi = 
  HttpApi 
  { httpApiAuth    = toServant auth
  , httpApiUser    = toServant user
  , httpApiService = toServant service
  }

auth :: AuthApi (AsServerT KatipController)
auth = 
  AuthApi 
  { authApiRegistration = 
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["auth", "registration"])  
    . Auth.Registration.controller
  , authApiSignIn = 
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["auth", "signIn"])  
    . Auth.SignIn.controller
  , authApiRefreshToken = undefined   
  }

user :: UserApi (AsServerT KatipController)
user = 
  UserApi 
  { userApiLoadProfile =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["user", "loadProfile"])
    . User.LoadProfile.controller
  , userPatchProfile = \uid patch ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["user", "patchProfile"])
     (User.PatchProfile.controller uid patch)      
  }

service :: ServiceApi (AsServerT KatipController)
service =
  ServiceApi
  { serviceApiLoadCountries = \lang ->
    flip logExceptionM ErrorS $ 
     katipAddNamespace 
     (Namespace ["service", "loadContries"])
     (Service.LoadCountries.controller lang)
  }