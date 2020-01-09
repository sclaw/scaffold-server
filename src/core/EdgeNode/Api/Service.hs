{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Service (ServiceApi (..), WebApi (..), GoogleApi (..)) where

import Servant.API.Generic
import Servant.API

newtype ServiceApi route = 
        ServiceApi
        { _serviceApiWeb 
          :: route
          :- Description "web services"
          :> "web"   
          :> ToServant WebApi AsApi
        } deriving stock Generic  
         
newtype WebApi route = 
        WebApi 
        { _webApiGoogle
          :: route
          :- Description "google"
          :> "google"   
          :> ToServant GoogleApi AsApi
        } deriving stock Generic

newtype GoogleApi route = 
        GoogleApi
        { _googleApiLoadCountries
          :: route
          :- Description "load all countries"
          :> Get '[JSON] ()     
        } deriving stock Generic