{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module EdgeNode.Api.Http.Test (spec_api) where

import EdgeNode.Api.Admin
import EdgeNode.Api.File
import EdgeNode.Api.Provider
import EdgeNode.Transport.Provider

import Test.Hspec
import Data.Proxy
import Servant.API.Generic
import Servant.Swagger.Test
import Data.DeriveTH
import Test.QuickCheck.Extended
import Prelude hiding (String)
import Protobuf.Scalar
import Data.Aeson.WithField

derive makeArbitrary ''ProviderRegistration
derive makeArbitrary ''Branch
derive makeArbitrary ''String
derive makeArbitrary ''WithField

spec_api :: Spec
spec_api = 
  describe "Swagger spec for API v1" $
   context "ToJSON matches ToSchema (AdminApi)" $ do 
     validateEveryToJSON (genericApi (Proxy :: Proxy AdminApi))
     validateEveryToJSON (genericApi (Proxy :: Proxy FileApi))
     validateEveryToJSON (genericApi (Proxy :: Proxy ProviderApi))