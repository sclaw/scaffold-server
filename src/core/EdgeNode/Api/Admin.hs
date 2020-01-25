{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Admin (AdminApi (..)) where

import EdgeNode.Transport.Provider
import EdgeNode.Transport.Response

import Servant.API.Generic
import Servant.API
import Data.Aeson.Unit

data AdminApi route = 
     AdminApi 
     { _adminApiProviderRegister
       :: route
       :- "provider"
       :> "registter"
       :> ReqBody '[JSON] ProviderRegistration 
       :> Post '[JSON] (Response Unit)  
     } deriving stock Generic