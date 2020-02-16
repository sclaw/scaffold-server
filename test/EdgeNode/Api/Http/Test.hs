{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module EdgeNode.Api.Http.Test (spec_api) where

import EdgeNode.Api.Admin
import EdgeNode.Api.File
import EdgeNode.Api.Provider ()
import EdgeNode.Api.Search
import EdgeNode.Transport.Provider

import Test.Hspec
import Data.Proxy
import Servant.API.Generic
import Servant.Swagger.Test
import Data.DeriveTH
import Test.QuickCheck.Extended
import Prelude hiding (String)
import Protobuf.Scalar
import Data.Aeson.WithField.Extended ()

derive makeArbitrary ''ProviderRegistration 
derive makeArbitrary ''Branch
derive makeArbitrary ''String

spec_api :: Spec
spec_api = 
  describe "Swagger spec for API v1" $ do 
   context "ToJSON matches ToSchema (AdminApi)" $  
     validateEveryToJSON (genericApi (Proxy :: Proxy AdminApi))
   context "ToJSON matches ToSchema (FileApi)" $   
     validateEveryToJSON (genericApi (Proxy :: Proxy FileApi))
   context "ToJSON matches ToSchema (SearchApi)" $   
     validateEveryToJSON (genericApi (Proxy :: Proxy SearchApi))
 --  context "ToJSON matches ToSchema (ProviderApi)" $    
 --    validateEveryToJSON (genericApi (Proxy :: Proxy ProviderApi))