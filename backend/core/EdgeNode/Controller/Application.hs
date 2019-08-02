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
application = ApplicationApi { _applicationApiHttp = toServant httpApi }

httpApi :: HttpApi (AsServerT KatipController)
httpApi = 
  HttpApi 
  { _httpApiAuth    = toServant auth
  , _httpApiUser    = toServant user
  , _httpApiService = toServant service
  }

auth :: AuthApi (AsServerT KatipController)
auth = 
  AuthApi 
  { _authApiRegistration = 
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["auth", "registration"])  
    . Auth.Registration.controller
  , _authApiSignIn = 
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["auth", "signIn"])  
    . Auth.SignIn.controller
  , _authApiRefreshToken = undefined   
  }

user :: UserApi (AsServerT KatipController)
user = 
  UserApi 
  { _userApiLoadProfile =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["user", "loadProfile"])
    . User.LoadProfile.controller
  , _userPatchProfile = \uid patch ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["user", "patchProfile"])
     (User.PatchProfile.controller uid patch)
  , _userSaveQualification = \_ -> undefined         
  }

service :: ServiceApi (AsServerT KatipController)
service =
  ServiceApi
  { _serviceApiLoadCountries = \lang ->
    flip logExceptionM ErrorS $ 
     katipAddNamespace 
     (Namespace ["service", "loadContries"])
     (Service.LoadCountries.controller lang)
  }