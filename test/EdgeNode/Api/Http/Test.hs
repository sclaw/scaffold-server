{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module EdgeNode.Api.Http.Test (spec_api) where

import EdgeNode.Api.Admin
import EdgeNode.Api.File
import EdgeNode.Api.Provider ()
import EdgeNode.Api.Search
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Extended
import EdgeNode.Statement.Provider ()
import EdgeNode.Transport.Search

import TH.Mk
import Test.Hspec
import Data.Proxy
import Servant.API.Generic
import Servant.Swagger.Test
import Data.Aeson.WithField.Extended ()

mkArbitrary ''ProviderRegistration
mkArbitrary ''GetBranchResp
mkArbitrary ''MkBranchReq
mkArbitrary ''PatchBranchReq
mkArbitrary ''SearchBar

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