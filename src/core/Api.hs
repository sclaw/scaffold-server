{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Api (Application (..), HomeApi (..), api) where


import           Servant.API.Generic
import           Servant.API.WebSocket
import           Data.Proxy
import           Servant.API

newtype Application route = ApplicationApi { home :: route :- ToServant HomeApi AsApi } deriving stock Generic

newtype HomeApi route = 
        HomeApi 
        { getAllIntegers 
          :: route
          :- "getAllIntegers" 
          :> WebSocket 
        } deriving stock Generic

api :: Proxy (ToServantApi Application)
api = genericApi (Proxy :: Proxy Application)

