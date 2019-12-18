module EdgeNode.Api.Http.Test (spec_api) where

import EdgeNode.Api.Http.Admin

import Test.Hspec
import Data.Proxy
import Servant.API.Generic
import Servant.Swagger.Test

spec_api :: Spec
spec_api = 
  describe "Swagger spec for API v1" $
   context "ToJSON matches ToSchema (AdminApi)" $
     validateEveryToJSON
     (genericApi (Proxy :: Proxy AdminApi))