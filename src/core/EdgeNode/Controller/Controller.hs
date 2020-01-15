{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module EdgeNode.Controller.Controller (controller) where

import Auth
import EdgeNode.Api
import EdgeNode.Transport.Id
import qualified EdgeNode.Model.Rbac as Rbac
import qualified EdgeNode.Statement.Rbac as Rbac
-- controllers
import qualified EdgeNode.Controller.File.Upload as File.Upload

import Katip
import KatipController
import Servant.Server.Generic
import Servant.API.Generic
import Servant.Auth.Server
import Network.IP.Addr
import Control.Lens
import Servant.Server
import Database.Transaction
import Control.Monad
import qualified Network.Wai as Wai
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types

controller :: ApplicationApi (AsServerT KatipController)
controller = ApplicationApi { _applicationApiHttp = toServant httpApi }

verifyAuthorization :: Id -> Rbac.Permission -> KatipController a -> KatipController a
verifyAuthorization uid perm controller = 
  do
    hasql <- (^.katipEnv.hasqlDbPool) `fmap` ask
    x <- katipTransaction hasql $ do 
      xs <- statement Rbac.getTopLevelRoles (uid, perm)
      statement Rbac.elem (xs, perm)
    case x of 
      Just isOk -> do 
        unless isOk (throwAll err403)
        controller
      Nothing -> throwAll err403
 
httpApi :: HttpApi (AsServerT KatipController)
httpApi = 
  HttpApi 
  { _httpApiAuth    = toServant . auth
  , _httpApiUser    = \ip -> (`authGateway` (toServant . user ip))
  , _httpApiService = \ip -> (`authGateway` (toServant . service ip))
  , _httpApiSearch  = \ip -> (`authGateway` (toServant . search ip))
  , _httpApiFile = toServant . file
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
  { _fileApiUpload = \bucket file -> 
    flip logExceptionM ErrorS $
    katipAddNamespace 
    (Namespace ["file", "upload"])
    (File.Upload.controller bucket file)
  , _fileApiPatch = undefined
  , _fileApiDelete = undefined
  , _fileApiDownload = Tagged $ \_ resp -> resp $ Wai.responseLBS status200 [] BL.empty
  } 
