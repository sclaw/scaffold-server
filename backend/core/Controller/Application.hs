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

application :: ApplicationApi (AsServerT KatipHandler)
application = 
  ApplicationApi 
  { root = Root.controller 
  , home = toServant (UserApi { register = Register.controller } :: UserApi (AsServerT KatipHandler))  
  }