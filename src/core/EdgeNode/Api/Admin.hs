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
import Data.Aeson.WithField

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
       :> "password"
       :> "reset"
       :> ReqBody '[JSON] (OnlyField "email" T.Text)
       :> Post '[JSON] (Response T.Text)
     } deriving stock Generic