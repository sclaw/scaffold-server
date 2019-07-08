{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EdgeNode.Controller.Application (application) where

import           EdgeNode.Api

import           Katip
import           KatipController
import           Servant.Server.Generic
import           Servant.API.Generic
import qualified EdgeNode.Controller.Root.Root as Root
import qualified EdgeNode.Controller.Auth.Register as Register 
import qualified EdgeNode.Controller.Auth.Authenticate as Authenticate
import qualified EdgeNode.Controller.V1.About as V1.About

application :: ApplicationApi (AsServerT KatipController)
application = 
  ApplicationApi 
  { applicationApiHttp = toServant httpApi 
  , applicationApiSocket = toServant socketApi  
  }

socketApi :: WebsocketApi (AsServerT KatipController)
socketApi = WebsocketApi { websocketApiAuth = toServant authApi }

authApi :: AuthApi (AsServerT KatipController)
authApi = 
  AuthApi 
  { authApiRegister = 
    katipAddNamespace (Namespace ["register"]) 
    . Register.controller
  , authApiAuthenticate = 
    katipAddNamespace (Namespace ["authenticate"]) 
    . Authenticate.controller
  }

httpApi :: HttpApi (AsServerT KatipController)
httpApi = 
  HttpApi 
  { httpApiRoot = katipAddNamespace (Namespace ["root"]) Root.controller
  , httpApiV1 = katipAddNamespace (Namespace ["api", "v1"]) (toServant v1Api)
  }

v1Api :: V1Api (AsServerT KatipController)
v1Api = V1Api { v1ApiAbout = katipAddNamespace (Namespace ["about"]) V1.About.controller }
