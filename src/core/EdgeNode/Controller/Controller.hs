{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Controller (controller) where

import Auth
import EdgeNode.Api
import EdgeNode.Model.User
import qualified EdgeNode.Model.Rbac as Rbac
import qualified EdgeNode.Statement.Rbac as Rbac
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
import Network.IP.Addr
import Control.Lens
import Servant.Server
import qualified Hasql.Session as Hasql.Session
import Database.Action
import Control.Monad

controller :: ApplicationApi (AsServerT KatipController)
controller = ApplicationApi { _applicationApiHttp = toServant httpApi }

verifyAuthorization :: UserId -> Rbac.Permission -> KatipController a -> KatipController a
verifyAuthorization uid perm controller = 
  do
    hasql <- (^.katipEnv.hasqlDb) `fmap` ask
    let action logger = 
          do xs <- Hasql.Session.statement (uid, perm) Rbac.getTopLevelRoles
             Hasql.Session.statement (xs, perm) Rbac.elem
    x <- runTryDbConnHasql action hasql
    let err e = do $(logTM) ErrorS (logStr (show e)); throwAll err500
    let ok (Just isOk) = do unless isOk (throwAll err403); controller
        ok Nothing = throwAll err403
    either err ok x
 
httpApi :: HttpApi (AsServerT KatipController)
httpApi = 
  HttpApi 
  { _httpApiAuth    = toServant . auth
  , _httpApiUser    = \ip -> (`authGateway` (toServant . user ip))
  , _httpApiService = \ip -> (`authGateway` (toServant . service ip))
  , _httpApiSearch  = \ip -> (`authGateway` (toServant . search ip))
  }

auth :: Maybe IP4 ->  AuthApi (AsServerT KatipController)
auth _ = 
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

user :: Maybe IP4 -> AuthResult JWTUser -> UserApi (AsServerT KatipController)
user _ user = 
  UserApi 
  { _userApiLoadProfile =
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "loadProfile"])
    (applyController Nothing user (\u -> 
     verifyAuthorization 
     (jWTUserUserId u) 
     Rbac.User 
     (User.LoadProfile.controller (jWTUserUserId u))))
  , _userPatchProfile =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "patchProfile"])
    . (\patch -> applyController Nothing user (\u -> 
        verifyAuthorization 
        (jWTUserUserId u) 
        Rbac.User 
        (User.PatchProfile.controller patch (jWTUserUserId u))))
  , _userSaveQualifications =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "saveQualifications"])
    . (\req -> applyController Nothing user (\u -> 
        verifyAuthorization 
        (jWTUserUserId u) 
        Rbac.User 
        (User.SaveQualifications.controller req (jWTUserUserId u))))
  , _userGetFullInfoQualififcation =
    flip logExceptionM ErrorS 
    . katipAddNamespace 
      (Namespace ["user", "getQualififcationFullInfo"])
    . (\qid -> applyController Nothing user (\u -> 
        verifyAuthorization 
        (jWTUserUserId u) 
        Rbac.User 
        (User.GetQualificationFullInfo.controller qid (jWTUserUserId u))))
  , _userGetCategories =
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "getCategories"])
    (applyController Nothing user (\u -> 
     verifyAuthorization 
     (jWTUserUserId  u) 
     Rbac.User 
     User.GetCategories.controller))
  , _userGetProvider = \cntry catType catId cursor ->
    flip logExceptionM ErrorS $
      katipAddNamespace 
      (Namespace ["user", "getProviders"])
      (applyController Nothing user (\u ->  
       verifyAuthorization 
       (jWTUserUserId u) 
       Rbac.User 
       (User.GetProviders.controller cntry catType catId cursor)))
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
     (applyController Nothing user (\u -> 
      verifyAuthorization 
      (jWTUserUserId u) 
      Rbac.User 
      (User.GetTrajectories.controller (jWTUserUserId u))))
  , _userSaveTrajectory =
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "saveTrajectory"])
    . (\req -> applyController Nothing user (\u -> 
        verifyAuthorization 
        (jWTUserUserId u) 
        Rbac.User 
        (User.SaveTrajectory.controller req (jWTUserUserId u))))
  , _userPatchQualifications = 
    flip logExceptionM ErrorS
    . katipAddNamespace 
      (Namespace ["user", "patchQualifications"])
    . (\req -> applyController Nothing user (\u -> 
        verifyAuthorization 
        (jWTUserUserId u) 
        Rbac.User 
        (User.PatchQualifications.controller req (jWTUserUserId u))))
  , _userDeleteQualification =   
    flip logExceptionM ErrorS
    . katipAddNamespace 
    (Namespace ["user", "deleteQualifications"])
    . (\id -> applyController Nothing user (\u -> 
        verifyAuthorization 
        (jWTUserUserId u) 
        Rbac.User 
        (User.DeleteQualification.controller id (jWTUserUserId u))))
  , _userDeleteTrajectory =
    flip logExceptionM ErrorS
    . katipAddNamespace 
    (Namespace ["user", "deleteTrajectory"])
    . (\id -> applyController Nothing user (\u -> 
        verifyAuthorization 
        (jWTUserUserId u) 
        Rbac.User 
        (User.DeleteTrajectory.controller id (jWTUserUserId u))))    
  }

service :: Maybe IP4 -> AuthResult JWTUser -> ServiceApi (AsServerT KatipController)
service _ user = ServiceApi { _serviceApiWeb = toServant webApi }
  where
    webApi ::WebApi (AsServerT KatipController)
    webApi = WebApi { _webApiGoogle = toServant googleApi }
    googleApi ::GoogleApi (AsServerT KatipController)
    googleApi =
      GoogleApi
      { _googleApiLoadCountries =
        flip logExceptionM ErrorS 
        . katipAddNamespace 
          (Namespace ["service", "web", "google", "loadContries"])
        . applyController Nothing user 
        . const . Service.LoadCountries.controller
      }
      
search :: Maybe IP4 -> AuthResult JWTUser -> SearchApi (AsServerT KatipController)
search _ user = 
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