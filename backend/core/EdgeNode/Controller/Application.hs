{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EdgeNode.Controller.Application (application) where

import EdgeNode.Api
-- controllers
import qualified EdgeNode.Controller.Http.Registration as Auth.Registration

import Katip
import KatipController
import Servant.Server.Generic
import Servant.API.Generic

application :: ApplicationApi (AsServerT KatipController)
application = ApplicationApi { applicationApiHttp = toServant httpApi }

httpApi :: HttpApi (AsServerT KatipController)
httpApi = HttpApi { httpApiAuth = toServant auth }


auth :: AuthApi (AsServerT KatipController)
auth = AuthApi 
      { authApiRegistration = 
        flip logExceptionM ErrorS 
        . katipAddNamespace 
          (Namespace ["auth", "registration"])  
        . Auth.Registration.controller 
      }