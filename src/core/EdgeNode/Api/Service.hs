{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Service (ServiceApi (..), EnumApi (..)) where

import EdgeNode.Transport.Response

import TH.Proto
import Servant.API.Generic
import Servant.API
import qualified Data.Text as T
import Data.Aeson.WithField

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
        { _enumApiGetCountry
          :: route
          :- Description "get enum country with its translation"
          :> "country"
          :> Capture "language" EdgeNodeLanguage
          :> Get '[JSON]
             (Response
              (WithField "enum"
               EdgeNodeCountry
               (OnlyField "value" T.Text)))
        } deriving stock Generic