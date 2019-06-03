{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Controller.Application (application) where

import           Api
import           Katip
import           KatipController
import           Servant.Server.Generic
import           Servant.API.Generic
import qualified Controller.Root.Root as Root
import qualified Controller.User.Register as Register 
import qualified Controller.User.Authenticate as Authenticate 
import qualified Controller.User.CheckLoginAvailability  as CheckLogin 
import qualified Controller.User.CheckEmailAvailability  as CheckEmail 


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
  , checkLoginAvailability = 
    katipAddNamespace (Namespace ["checkLoginAvailability"]) 
    . CheckLogin.controller
  , checkEmailAvailability = 
    katipAddNamespace (Namespace ["checkEmailAvailability"]) 
    . CheckEmail.controller
  }