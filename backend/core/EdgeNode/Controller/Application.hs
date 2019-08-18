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
import qualified EdgeNode.Controller.Http.SaveQualifications as User.SaveQualifications
import qualified EdgeNode.Controller.Http.GetQualificationFullInfo as User.GetQualificationFullInfo
import qualified EdgeNode.Controller.Http.RefreshToken as Auth.RefreshToken
import qualified EdgeNode.Controller.Http.GetCategories as User.GetCategories
import qualified EdgeNode.Controller.Http.GetProviders as User.GetProviders
import qualified EdgeNode.Controller.Http.GetQualififcations as User.GetQualififcations 

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
  , _userSaveQualifications =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "saveQualifications"])
    . User.SaveQualifications.controller (jWTUserUserId user)
  , _userGetFullInfoQualififcation =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["user", "getQualififcationFullInfo"])
    . (User.GetQualificationFullInfo.controller 
      (jWTUserUserId user))
  , _userGetCategories =
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "getCategories"])
    User.GetCategories.controller
  , _userGetProvider =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "getProviders"])
    . User.GetProviders.controller
  , _userGetQualififcations =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "getQualififcations"])
    . User.GetQualififcations.controller    
  }

service :: ServiceApi (AsServerT KatipController)
service =
  ServiceApi
  { _serviceApiLoadCountries =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["service", "loadContries"])
    . Service.LoadCountries.controller
  }