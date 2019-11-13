{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Service (ServiceApi (..), WebApi (..), GoogleApi (..)) where

import Web.Google.Translator

import Servant.API.Generic
import Servant.API
import Json
import Proto
import qualified Data.Text as T

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
          :> "countries" 
          :> Capture "lang" GoogleLanguage
          :> Get '[JSON] (Alternative (Error T.Text) CountryResponse)     
        } deriving stock Generic