{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Controller.Application (application) where

import           Api
import           KatipHandler
import           Servant.Server.Generic
import           Servant.API.Generic
import qualified Controller.Root.Root as Root
import qualified Controller.User.Register as Register 
import qualified Controller.User.Authorize as Authorize 
import qualified Controller.User.CheckLoginAvailability  as CheckLogin 
import qualified Controller.User.CheckEmailAvailability  as CheckEmail 

application :: ApplicationApi (AsServerT KatipHandler)
application = 
  ApplicationApi 
  { root = Root.controller 
  , home = toServant user  
  }

user :: UserApi (AsServerT KatipHandler)
user = UserApi 
       { register = Register.controller
       , authorize = Authorize.controller
       , checkLoginAvailability = CheckLogin.controller
       , checkEmailAvailability = CheckEmail.controller
       }