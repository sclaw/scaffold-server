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
  { root = Root.controller 
  , auth = toServant authApi  
  }

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