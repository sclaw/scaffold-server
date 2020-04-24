{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Service (ServiceApi (..), EnumApi (..)) where

import EdgeNode.Transport.Response
import EdgeNode.Country

import TH.Proto
import Servant.API.Generic
import Servant.API
import qualified Data.Text as T
import Data.Aeson.WithField
import Data.Swagger.Schema

instance ToSchema Country

newtype ServiceApi route =
        ServiceApi
        { _serviceApiEnum
          :: route
          :- Description "enum api"
          :> "enum"
          :> ToServant EnumApi AsApi
        } deriving stock Generic

newtype EnumApi route =
        EnumApi
        { _enumApiGetCountries
          :: route
          :- Description "get enum country with its translation"
          :> "country"
          :> Capture "language" EdgeNodeLanguage
          :> Get '[JSON] (Response [WithField "enum" Country T.Text])
        } deriving stock Generic