{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Service (ServiceApi (..), EnumApi (..)) where

import EdgeNode.Transport.Response
import EdgeNode.Country
import EdgeNode.Transport.Provider.Qualification

import TH.Proto
import Servant.API.Generic
import Servant.API
import qualified Data.Text as T
import Data.Aeson.WithField
import Data.Aeson.Unit
import Data.Swagger.Schema
import Servant.Ip

instance ToSchema Country

data ServiceApi route =
     ServiceApi
     { _serviceApiEnum
       :: route
       :- Description "enum api"
       :> "enum"
       :> ToServant EnumApi AsApi
     , _serviceApiTouchVisitor
       :: route
       :- Description "visitor"
       :> HeaderIP
       :> "visitor"
       :> Post '[JSON] (Response Unit)
     } deriving stock Generic

data EnumApi route =
     EnumApi
     { _enumApiGetCountries
       :: route
       :- Description "get enum country with its translation"
       :> "country"
       :> Capture "language" EdgeNodeLanguage
       :> Get '[JSON] (Response [WithField "enum" Country T.Text])
     , _enumApiGetQualificationEnum
       :: route
       :- Description "all enums from qualififcation"
       :> "qualification"
       :> Capture "language" EdgeNodeLanguage
       :> Get '[JSON] (Response Enums)
     } deriving stock Generic