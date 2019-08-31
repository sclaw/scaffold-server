{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified EdgeNode.Controller.Http.SearchQualification as SearchQualification
import qualified EdgeNode.Controller.Http.SearchInit as SearchInit
import qualified EdgeNode.Controller.Http.GetTrajectories as User.GetTrajectories
import qualified EdgeNode.Controller.Http.SaveTrajectory as User.SaveTrajectory

import Katip
import KatipController
import Servant.Server.Generic
import Servant.API.Generic
import Servant.Auth.Server


application :: ApplicationApi (AsServerT KatipController)
application = ApplicationApi { _applicationApiHttp = toServant httpApi }

httpApi :: HttpApi (AsServerT KatipController)
httpApi = 
  HttpApi 
  { _httpApiAuth    = toServant auth
  , _httpApiUser    = (`authGateway` (toServant . user))
  , _httpApiService = (`authGateway` (toServant . service))
  , _httpApiSearch  = toServant search
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
    . (applyController Auth.SignOut.controller)
  }

user :: AuthResult JWTUser -> UserApi (AsServerT KatipController)
user user = 
  UserApi 
  { _userApiLoadProfile =
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "loadProfile"])
    (applyController 
     ( User.LoadProfile.controller 
     . jWTUserUserId) user)
  , _userPatchProfile =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "patchProfile"])
    . (\patch -> applyController (User.PatchProfile.controller patch . jWTUserUserId) user)
  , _userSaveQualifications =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "saveQualifications"])
    . (\req -> applyController (User.SaveQualifications.controller req . jWTUserUserId) user)
  , _userGetFullInfoQualififcation =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["user", "getQualififcationFullInfo"])
    . (\qid -> applyController (User.GetQualificationFullInfo.controller qid . jWTUserUserId) user)
  , _userGetCategories =
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "getCategories"])
    (applyController (const User.GetCategories.controller) user)
  , _userGetProvider = \cntry catType catId cursor ->
    flip logExceptionM ErrorS $
      katipAddNamespace 
      (Namespace ["user", "getProviders"])
      (applyController (const (User.GetProviders.controller cntry catType catId cursor)) user)
  , _userGetQualififcations =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "getQualififcations"])
    . (\pid -> applyController (const (User.GetQualififcations.controller pid)) user)
  , _userGetTrajectories =
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["user", "getTrajectories"])
     (applyController (User.GetTrajectories.controller . jWTUserUserId) user)
  , _userSaveTrajectory =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "saveTrajectory"])
    . (\req -> applyController (User.SaveTrajectory.controller req . jWTUserUserId) user)  
  }

service :: AuthResult JWTUser -> ServiceApi (AsServerT KatipController)
service user =
  ServiceApi
  { _serviceApiLoadCountries =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["service", "loadContries"])
    . (\lang -> applyController (const (Service.LoadCountries.controller lang)) user)
  }

search :: SearchApi (AsServerT KatipController)
search = 
  SearchApi 
  { _searchApiSearch = \piece query ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["search", "searchQualification"])
     (SearchQualification.controller piece query)
  , _searchApiInit = 
     flip logExceptionM ErrorS $
      katipAddNamespace 
      (Namespace ["search", "init"])
      SearchInit.controller
  }