{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Statistics (StatisticsApi (..)) where

import EdgeNode.Transport.Response

import Servant.API.Generic
import Servant.API
import qualified Data.Text as T
import Data.Aeson.WithField.Extended
import Data.Int

data StatisticsApi route =
     StatisticsApi
     { _statisticsApiGetRegistrations
       :: route
       :- Description "registration per day"
       :> "registration"
       :> QueryParam "from" (OnlyField "from" Int32)
       :> Get '[JSON] (Response [WithField "day" T.Text (OnlyField "count" Int32)])
     , _statisticsApiGetActiveUsers
       :: route
       :- Description "active users per day"
       :> "activeUsers"
       :> QueryParam "from" (OnlyField "from" Int32)
       :> Get '[JSON] (Response [WithField "day" T.Text (OnlyField "count" Int32)])
     } deriving stock Generic