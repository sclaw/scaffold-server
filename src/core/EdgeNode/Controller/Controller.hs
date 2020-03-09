{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Controller (controller) where

import Auth
import EdgeNode.Api
import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import qualified EdgeNode.Model.Rbac as Rbac
import qualified EdgeNode.Statement.Rbac as Rbac
-- controllers
import qualified EdgeNode.Controller.File.Upload as File.Upload
import qualified EdgeNode.Controller.File.Download as File.Download
import qualified EdgeNode.Controller.File.Delete as File.Delete
import qualified EdgeNode.Controller.File.Patch as File.Patch
import qualified EdgeNode.Controller.Admin.ProviderRegister as Admin.ProviderRegister
import qualified EdgeNode.Controller.Admin.ResetPassword as Admin.ResetPassword 
import qualified EdgeNode.Controller.Provider.GetBranches as Provider.GetBranches 
import qualified EdgeNode.Controller.Provider.CreateBranches as Provider.CreateBranches 
import qualified EdgeNode.Controller.Provider.PatchBranch as Provider.PatchBranch 
import qualified EdgeNode.Controller.Provider.DeleteBranch as Provider.DeleteBranch 
import qualified EdgeNode.Controller.Provider.SetHQ as Provider.SetHQ
import qualified EdgeNode.Controller.Search as Search
import qualified EdgeNode.Controller.Auth.SignIn as Auth.SignIn 
import qualified EdgeNode.Controller.Auth.RefreshAccessToken as Auth.RefreshAccessToken
import qualified EdgeNode.Controller.Provider.Publish as Provider.Publish
import qualified EdgeNode.Controller.Provider.QualificationBuilder.GetAvailableBranches as QualificationBuilder.GetAvailableBranches
import qualified EdgeNode.Controller.Provider.QualificationBuilder.Create as QualificationBuilder.Create
import qualified EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries as QualificationBuilder.GetAreaToCountries
import qualified EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes as QualificationBuilder.GetCountryToTypes
import qualified EdgeNode.Controller.Provider.QualificationBuilder.GetTypeToQualifications as QualificationBuilder.GetTypeToQualifications

import Katip
import KatipController
import Servant.Server.Generic
import Servant.API.Generic
import Servant.Auth.Server
import Network.IP.Addr
import Control.Lens
import Database.Transaction
import Data.Bool
import qualified Data.Text as T

controller :: ApplicationApi (AsServerT KatipController)
controller = ApplicationApi { _applicationApiHttp = toServant httpApi }

verifyAuthorization :: Id "user" -> Rbac.Permission -> (Id "user" -> KatipController (Response a)) -> KatipController (Response a)
verifyAuthorization uid perm controller = 
  do
    hasql <- (^.katipEnv.hasqlDbPool) `fmap` ask
    isOk <- katipTransaction hasql $ do 
      xs <- statement Rbac.getTopLevelRoles (uid, perm)
      statement Rbac.isPermissionBelongToRole (xs, perm)
    let error = "you have no permissions to fulfill this operation"  
    bool (return (Error (asError @T.Text error))) 
         (controller uid) isOk
 
httpApi :: HttpApi (AsServerT KatipController)
httpApi = 
  HttpApi 
  { _httpApiAuth    = toServant . auth
  , _httpApiUser    = \ip -> (`withAuthResult` (toServant . user ip))
  , _httpApiService = \ip -> (`withAuthResult` (toServant . service ip))
  , _httpApiSearch  = toServant . search
  , _httpApiFile = toServant . file
  , _httpApiAdmin = \ip -> (`withAuthResult` (toServant . admin ip))
  , _httpApiProvider = \ip -> (`withAuthResult` (toServant . provider ip))
  }

auth :: Maybe IP4 ->  AuthApi (AsServerT KatipController)
auth _ = 
  AuthApi 
  { _authApiAuthentication = \req -> 
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["auth", "authentication"])  
    (Auth.SignIn.controller req)
  , _authApiRefreshAccessToken = \uid req ->
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["auth", "refreshAccessToken"])  
    (Auth.RefreshAccessToken.controller req uid) 
  }

user :: Maybe IP4 -> AuthResult JWTUser -> UserApi (AsServerT KatipController)
user _ user = 
  UserApi 
  { _userApiLoadProfile =
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["user", "loadProfile"])
    undefined
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
        flip logExceptionM ErrorS $ 
        katipAddNamespace 
        (Namespace ["service", "web", "google", "loadContries"])
        undefined
      }
      
search :: Maybe IP4 -> SearchApi (AsServerT KatipController)
search _ = 
  SearchApi 
  { _searchApiSearch = \query ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["search"])
     (Search.controller query)
  }

file :: Maybe IP4 -> FileApi (AsServerT KatipController)
file _ = 
  FileApi 
  { _fileApiUpload = \bucket files -> 
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["file", "upload"])
    (File.Upload.controller bucket files)
  , _fileApiPatch = \fid file ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["file", "patch"])
    (File.Patch.controller fid file)  
  , _fileApiDelete = \fid ->
     flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["file", "delete"]) 
     (File.Delete.controller fid)    
  , _fileApiDownload = \option fid -> 
     flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["file", "download"]) 
     (File.Download.controller option fid)
  }

admin :: Maybe IP4 -> AuthResult BasicUser -> AdminApi (AsServerT KatipController)
admin _ user = 
  AdminApi 
  { _adminApiProviderRegister = \provider -> 
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["admin", "provider", "register"])
     (withUser user (Admin.ProviderRegister.controller provider))
  , _adminApiResetPassword = \provider_id user_id ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["admin", "provider", "password", "reset"])
     (withUser user (const (Admin.ResetPassword.controller provider_id user_id)))
  }

provider  
 :: Maybe IP4 
 -> AuthResult JWTUser 
 -> ProviderApi (AsServerT KatipController)
provider _ user = 
  ProviderApi
  { _providerApiGetBranches = 
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "branch", "list"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderGuest 
       Provider.GetBranches.controller)
  , _providerApiCreateBranches = \branch ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "branch", "create"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderAdmin 
       (Provider.CreateBranches.controller branch))    
  , _providerApiPatchBranches = \branches ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "branch", "patch"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderAdmin 
       (Provider.PatchBranch.controller branches))    
  , _providerApiDeleteBranch = \ident ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "branch", "delete"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderAdmin 
       (Provider.DeleteBranch.controller ident))
  , _providerApiSetHQ = \ident ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "branch", "hq"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderAdmin 
       (Provider.SetHQ.controller ident))
  , _providerApiBuilderCreateQualification = \builder -> 
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "qualification", "builder", "create"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderAdmin
       (const (QualificationBuilder.Create.controller builder)))    
  , _providerApiBuilderGetAvailableBranches =
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "qualification", "builder", "getBranches"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderAdmin
       QualificationBuilder.GetAvailableBranches.controller)
  , _providerApiPublish =
    flip logExceptionM ErrorS $
    katipAddNamespace 
     (Namespace ["provider", "branch", "publish"])    
     (applyController Nothing user $ \x -> 
      verifyAuthorization 
      (jWTUserUserId x) 
      Rbac.PermissionProviderAdmin
      Provider.Publish.controller)
  , _providerApiBuilderGetDependencyAreaToCountries = \area ->
    flip logExceptionM ErrorS $
    katipAddNamespace 
     (Namespace ["provider", "qualification", "builder", "areaToCountries"])    
     (applyController Nothing user $ \x -> 
      verifyAuthorization 
      (jWTUserUserId x) 
      Rbac.PermissionProviderAdmin
      (const (QualificationBuilder.GetAreaToCountries.controller area))) 
  , _providerApiBuilderGetDependencyCountryToTypes = \area cntry ->
    flip logExceptionM ErrorS $
    katipAddNamespace 
     (Namespace ["provider", "qualification", "builder", "CountryToTypes"])    
     (applyController Nothing user $ \x -> 
      verifyAuthorization 
      (jWTUserUserId x) 
      Rbac.PermissionProviderAdmin
      (const (QualificationBuilder.GetCountryToTypes.controller area cntry)))
  , _providerApiBuilderGetDependencTypeToQualifications = \area cntry degree ->
    flip logExceptionM ErrorS $
    katipAddNamespace 
     (Namespace ["provider", "qualification", "builder", "TypeToQualifications"])    
     (applyController Nothing user $ \x -> 
      verifyAuthorization 
      (jWTUserUserId x) 
      Rbac.PermissionProviderAdmin
      (const (QualificationBuilder.GetTypeToQualifications.controller area cntry degree)))
  }