{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Controller.Application (application) where

import           Api
import           Katip
import           KatipController
import           Servant.Server.Generic
import           Servant.API.Generic
import qualified Controller.Root.Root as Root
import qualified Controller.Auth.Register as Register 
import qualified Controller.Auth.Authenticate as Authenticate

application :: ApplicationApi (AsServerT KatipController)
application = 
  ApplicationApi 
  { http = toServant httpApi 
  , socket = toServant socketApi  
  }

socketApi :: WebsocketApi (AsServerT KatipController)
socketApi = WebsocketApi { auth = toServant authApi }

authApi :: AuthApi (AsServerT KatipController)
authApi = 
  AuthApi 
  { register = 
    katipAddNamespace (Namespace ["register"]) 
    . Register.controller
  , authenticate = 
    katipAddNamespace (Namespace ["authenticate"]) 
    . Authenticate.controller
  }

httpApi :: HttpApi (AsServerT KatipController)
httpApi = HttpApi { root = katipAddNamespace (Namespace ["root"]) Root.controller }

