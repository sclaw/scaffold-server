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
import qualified EdgeNode.Controller.Http.SearchQualification as SearchQualification
import qualified EdgeNode.Controller.Http.GetTrajectories as User.GetTrajectories
import qualified EdgeNode.Controller.Http.SaveTrajectory as User.SaveTrajectory

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
  , _httpApiService = (`authGateway` const (toServant service))
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
  , _userGetProvider = \cntry catType catId cursor ->
    flip logExceptionM ErrorS $
      katipAddNamespace 
      (Namespace ["user", "getProviders"])
      (User.GetProviders.controller cntry catType catId cursor)
  , _userGetQualififcations =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "getQualififcations"])
    . User.GetQualififcations.controller
  , _userGetTrajectories =
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["user", "getTrajectories"])
     User.GetTrajectories.controller
  , _userSaveTrajectory =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "saveTrajectory"])
    . User.SaveTrajectory.controller (jWTUserUserId user)   
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

search :: SearchApi (AsServerT KatipController)
search = 
  SearchApi 
  { _searchApiSearch = \piece ->
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["provider", "searchQualification"])
    . SearchQualification.controller piece
  }