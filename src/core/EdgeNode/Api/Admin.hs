{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Admin (AdminApi (..)) where

import Servant.API.Generic
import Servant.API

data AdminApi route = 
     AdminApi 
     { _adminApiRegistrationCountry
       :: route
       :- "provider"
       :> "registration"
       :> "country"
       :> Get '[JSON] ()
     , _adminApiRegistrationTypes
       :: route
       :- "provider"
       :> "registration"
       :> "types"
       :> Get '[JSON] ()  
     } deriving stock Generic