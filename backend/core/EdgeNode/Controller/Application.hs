{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EdgeNode.Controller.Application (application) where

import           EdgeNode.Api

import           Katip
import           KatipController
import           Servant.Server.Generic
import           Servant.API.Generic
import qualified EdgeNode.Controller.Http.About as Http.About

application :: ApplicationApi (AsServerT KatipController)
application = 
  ApplicationApi 
  { applicationApiHttp = toServant httpApi   
  }

httpApi :: HttpApi (AsServerT KatipController)
httpApi = HttpApi { httpApiAbout = \_ -> katipAddNamespace (Namespace ["http", "about"]) Http.About.controller }