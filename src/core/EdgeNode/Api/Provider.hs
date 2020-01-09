{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Provider (ProviderApi (..)) where

import Servant.API.Generic
import Servant.API

data ProviderApi route = 
     ProviderApi 
     { _providerApiRegistrationCountry
       :: route
       :- "registration"
       :> "country"
       :> Get '[JSON] ()
     } deriving stock Generic