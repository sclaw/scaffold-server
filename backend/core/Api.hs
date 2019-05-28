{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Api (ApplicationApi (..), HomeApi (..), api) where


import           Servant.API.Generic
import           Servant.API.WebSocket
import           Data.Proxy
import           Servant.API

data ApplicationApi route = 
     ApplicationApi 
     { root :: route :-  Get '[JSON] String
     , home :: route :- ToServant HomeApi AsApi 
     } deriving stock Generic

newtype HomeApi route = 
        HomeApi 
        { getAllIntegers 
          :: route
          :- "getAllIntegers" 
          :> WebSocket 
        } deriving stock Generic

api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)

