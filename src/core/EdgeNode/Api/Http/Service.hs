{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Service (ServiceApi (..)) where

import EdgeNode.Service.Data 

import Servant.API.Generic
import Servant.API
import Json
import Proto
import qualified Data.Text as T

newtype ServiceApi route = 
        ServiceApi
       { _serviceApiLoadCountries
       :: route
       :- Description "load all countries"
       :> "countries" 
       :> Capture "lang" ServiceLanguage
       :> Get '[JSON] (Alternative (Error T.Text) CountriesResponse)     
     } deriving stock Generic
