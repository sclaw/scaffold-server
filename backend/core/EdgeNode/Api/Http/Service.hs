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
import ReliefJsonData
import RetrofitProto
import qualified Data.Text as T

newtype ServiceApi route = 
        ServiceApi
       { serviceApiLoadCountries
       :: route
       :- Description "load all countries"
       :> "countries" 
       :> Capture "lang" ServiceLanguage
       :> Post '[JSON] (Alternative T.Text CountriesResponse)     
     } deriving stock Generic
