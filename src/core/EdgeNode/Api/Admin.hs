{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Api.Admin (AdminApi (..)) where

import EdgeNode.Transport.Provider
import EdgeNode.Transport.Response

import Servant.API.Generic
import Servant.API.Extended
import Data.Aeson.Unit
import Servant.Server.Internal.ServantErr
import Data.Derive.Default
import Data.DeriveTH
import Data.Default
import Data.Text.Lazy
import Data.Aeson

instance Default Text where def = empty

derive makeDefault  ''ProviderRegistration

instance MakeCustomError "edgenode-error" ProviderRegistration where
  makeCustomError _ = err400 { errBody = "expected json: " <> encode (def :: ProviderRegistration) }

data AdminApi route = 
     AdminApi 
     { _adminApiProviderRegister
       :: route
       :- "provider"
       :> "register"
       :> ReqBodyCustomError 
          '[JSON] 
          "edgenode-error" 
          ProviderRegistration 
       :> Post '[JSON] (Response Unit)  
     } deriving stock Generic