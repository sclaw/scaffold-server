{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

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
import qualified EdgeNode.Controller.Provider.GetBranches as Provider.GetBranches 
import qualified EdgeNode.Controller.Provider.CreateBranch as Provider.CreateBranch 
import qualified EdgeNode.Controller.Provider.PatchBranch as Provider.PatchBranch 
import qualified EdgeNode.Controller.Provider.DeleteBranch as Provider.DeleteBranch 

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

verifyAuthorization :: Id -> Rbac.Permission -> (Id -> KatipController (Response a)) -> KatipController (Response a)
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
  , _httpApiSearch  = \ip -> (`withAuthResult` (toServant . search ip))
  , _httpApiFile = toServant . file
  , _httpApiAdmin = \ip -> (`undefined` (toServant . admin ip))
  , _httpApiProvider = \ip -> (`withAuthResult` (toServant . provider ip))
  }

auth :: Maybe IP4 ->  AuthApi (AsServerT KatipController)
auth _ = 
  AuthApi 
  { _authApiRegistration = 
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["auth", "registration"])  
    undefined
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
      
search :: Maybe IP4 -> AuthResult JWTUser -> SearchApi (AsServerT KatipController)
search _ user = 
  SearchApi 
  { _searchApiSearch =
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["search", "searchQualification"])
     undefined       
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
  , _fileApiDownload = \fid -> 
     flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["file", "download"]) 
     (File.Download.controller fid)
  }

admin :: Maybe IP4 -> AuthResult JWTUser -> AdminApi (AsServerT KatipController)
admin _ _ = 
  AdminApi 
  { _adminApiProviderRegister = \provider -> 
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["admin", "provider", "register"])
     (Admin.ProviderRegister.controller provider)
  }

provider :: Maybe IP4 -> AuthResult JWTUser -> ProviderApi (AsServerT KatipController)
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
  , _providerApiCreateBranch = \branch ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "branch", "create"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderAdmin 
       (Provider.CreateBranch.controller branch))    
  , _providerApiPatchBranch = \ident branch ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "branch", "patch"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderAdmin 
       (Provider.PatchBranch.controller ident branch))    
  , _providerApiDeleteBranch = \ident ->
    flip logExceptionM ErrorS $
     katipAddNamespace 
     (Namespace ["provider", "branch", "delete"])    
     (applyController Nothing user $ \x -> 
       verifyAuthorization 
       (jWTUserUserId x) 
       Rbac.PermissionProviderAdmin 
       (Provider.DeleteBranch.controller ident))    
  }