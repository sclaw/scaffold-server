{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Controller.Application (application) where

import Auth
import EdgeNode.Api
-- controllers
import qualified EdgeNode.Controller.Http.Registration as Auth.Registration
import qualified EdgeNode.Controller.Http.SignIn as Auth.SignIn
import qualified EdgeNode.Controller.Http.SignOut as Auth.SignOut
import qualified EdgeNode.Controller.Http.LoadProfile as User.LoadProfile 
import qualified EdgeNode.Controller.Http.PatchProfile as User.PatchProfile
import qualified EdgeNode.Controller.Http.LoadCountries as Service.LoadCountries
import qualified EdgeNode.Controller.Http.SaveQualification as User.SaveQualification
import qualified EdgeNode.Controller.Http.GetQualification as User.GetQualification
import qualified EdgeNode.Controller.Http.RefreshToken as Auth.RefreshToken

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
  , _httpApiUser    = (`authGateway` (toServant . user))
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
  , _authApiRefreshToken =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["auth", "refreshToken"])  
    . Auth.RefreshToken.controller
  , _authApiSignOut = 
    flip authGateway $ 
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["auth", "signOut"])  
    . Auth.SignOut.controller    
  }

user :: JWTUser -> UserApi (AsServerT KatipController)
user user = 
  UserApi 
  { _userApiLoadProfile =
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "loadProfile"])
    (User.LoadProfile.controller (jWTUserUserId user))
  , _userPatchProfile =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["user", "patchProfile"])
    . User.PatchProfile.controller (jWTUserUserId user)
  , _userSaveQualification = \_ -> 
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "SaveQualification"])
    User.SaveQualification.controller
  , _userGetQualififcation = \_ -> 
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "GetQualififcation"])
    User.GetQualification.controller
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