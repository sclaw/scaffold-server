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
import qualified EdgeNode.Controller.Http.SearchInit as Search.SearchInit
import qualified EdgeNode.Controller.Http.GetTrajectories as User.GetTrajectories
import qualified EdgeNode.Controller.Http.SaveTrajectory as User.SaveTrajectory
import qualified EdgeNode.Controller.Http.SearchFilter as Search.SearchFilter
import qualified EdgeNode.Controller.Http.SearchApplyFilter as Search.SearchApplyFilter
import qualified EdgeNode.Controller.Http.PatchQualifications as User.PatchQualifications
import qualified EdgeNode.Controller.Http.DeleteQualification as User.DeleteQualification
import qualified EdgeNode.Controller.Http.DeleteTrajectory as User.DeleteTrajectory
import qualified EdgeNode.Controller.Http.ValidateRefreshToken as Auth.ValidateRefreshToken 

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
  , _httpApiSearch  = (`authGateway` (toServant . search))
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
    . (\u -> applyController Nothing u Auth.SignOut.controller)
  , _authApiCheckRefreshToken = 
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["auth", "validateRefreshToken"])  
    . Auth.ValidateRefreshToken.controller    
  }

user :: AuthResult JWTUser -> UserApi (AsServerT KatipController)
user user = 
  UserApi 
  { _userApiLoadProfile =
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "loadProfile"])
    (applyController Nothing user (User.LoadProfile.controller . jWTUserUserId))
  , _userPatchProfile =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "patchProfile"])
    . (\patch -> applyController Nothing user (User.PatchProfile.controller patch . jWTUserUserId))
  , _userSaveQualifications =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "saveQualifications"])
    . (\req -> applyController Nothing user (User.SaveQualifications.controller req . jWTUserUserId))
  , _userGetFullInfoQualififcation =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["user", "getQualififcationFullInfo"])
    . (\qid -> applyController Nothing user (User.GetQualificationFullInfo.controller qid . jWTUserUserId))
  , _userGetCategories =
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "getCategories"])
    (applyController Nothing user (const User.GetCategories.controller))
  , _userGetProvider = \cntry catType catId cursor ->
    flip logExceptionM ErrorS $
      katipAddNamespace 
      (Namespace ["user", "getProviders"])
      (applyController Nothing user (const (User.GetProviders.controller cntry catType catId cursor)))
  , _userGetQualififcations =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "getQualififcations"])
    . applyController Nothing user 
    . const . User.GetQualififcations.controller
  , _userGetTrajectories =
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["user", "getTrajectories"])
     (applyController Nothing user (User.GetTrajectories.controller . jWTUserUserId))
  , _userSaveTrajectory =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "saveTrajectory"])
    . (\req -> applyController Nothing user (User.SaveTrajectory.controller req . jWTUserUserId))
  , _userPatchQualifications = 
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "patchQualifications"])
    . (\req -> applyController Nothing user (User.PatchQualifications.controller req . jWTUserUserId))
  , _userDeleteQualification =   
    flip logExceptionM ErrorS
    . katipAddNamespace 
    (Namespace ["user", "deleteQualifications"])
    . (\id -> applyController Nothing user (User.DeleteQualification.controller id . jWTUserUserId))
  , _userDeleteTrajectory =
    flip logExceptionM ErrorS
    . katipAddNamespace 
    (Namespace ["user", "deleteTrajectory"])
    . (\id -> applyController Nothing user (User.DeleteTrajectory.controller id . jWTUserUserId))    
  }

service :: AuthResult JWTUser -> ServiceApi (AsServerT KatipController)
service user =
  ServiceApi
  { _serviceApiLoadCountries =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["service", "loadContries"])
    . applyController Nothing user 
    . const . Service.LoadCountries.controller
  }

search :: AuthResult JWTUser -> SearchApi (AsServerT KatipController)
search user = 
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
      (applyController 
       (Just (Search.SearchInit.controller Nothing)) 
       user 
       (Search.SearchInit.controller . Just . jWTUserUserId))
  , _searchApiFilter =
     flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["search", "filter"])
     (applyController 
      (Just Search.SearchFilter.controller) 
      user 
      (const Search.SearchFilter.controller))
  , _searchApiApplyFilter = \filter ->
     flip logExceptionM ErrorS $
       katipAddNamespace 
       (Namespace ["search", "filter", "apply"])
       (applyController 
        (Just (Search.SearchApplyFilter.controller filter Nothing)) 
        user 
        (Search.SearchApplyFilter.controller filter . Just . jWTUserUserId))       
  }