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
import Servant.API
import qualified Data.Text as T

data AdminApi route = 
     AdminApi 
     { _adminApiProviderRegister
       :: route
       :- "provider"
       :> "register"
       :> ReqBody '[JSON] ProviderRegistration 
       :> Put '[JSON] (Response T.Text)
     , _adminApiResetPassword
       :: route
       :- "provider"
       :> Capture "provider_id" T.Text
       :> "email"
       :> Capture "email" T.Text
       :> "password"
       :> "reset" 
       :> Post '[JSON] (Response T.Text)
     } deriving stock Generic