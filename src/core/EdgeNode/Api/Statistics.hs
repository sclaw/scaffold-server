{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Statistics (StatisticsApi (..)) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Statistics

import Servant.API.Generic
import Servant.API
import Data.Aeson.WithField.Extended
import Data.Int

data StatisticsApi route =
     StatisticsApi
     { _statisticsApiGetRegistrations
       :: route
       :- Description "registration per day"
       :> "registrations"
       :> QueryParam "from" (OnlyField "from" Int32)
       :> Get '[JSON] (Response Items)
     , _statisticsApiGetActiveUsers
       :: route
       :- Description "active users per day"
       :> "users"
       :> QueryParam "from" (OnlyField "from" Int32)
       :> Get '[JSON] (Response Items)
     , _statisticsApiGetApiCounter
       :: route
       :- Description "api counter per day"
       :> "api"
       :> QueryParam "from" (OnlyField "from" Int32)
       :> Get '[JSON] (Response Items)
     } deriving stock Generic