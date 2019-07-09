{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EdgeNode.Controller.Application (application) where

import           EdgeNode.Api

import           Katip
import           KatipController
import           Servant.Server.Generic
import           Servant.API.Generic
import qualified EdgeNode.Controller.Socket.Register as Socket.Register 
import qualified EdgeNode.Controller.Socket.Authenticate as Socket.Authenticate
import qualified EdgeNode.Controller.Http.About as Http.About

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
    katipAddNamespace (Namespace ["socket", "register"]) 
    . Socket.Register.controller
  , authApiAuthenticate = 
    katipAddNamespace (Namespace ["socket", "authenticate"]) 
    . Socket.Authenticate.controller
  }

httpApi :: HttpApi (AsServerT KatipController)
httpApi = HttpApi { httpApiAbout = katipAddNamespace (Namespace ["http", "about"]) Http.About.controller }