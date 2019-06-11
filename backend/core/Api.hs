{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Api 
       ( ApplicationApi (..)
       , AuthApi (..)
       , HttpApi (..)
       , WebsocketApi (..)
       , api
       , swaggerHttpApi
       ) where

import           Servant.API.Generic
import           Servant.API.WebSocket
import           Data.Proxy
import           Servant.API
import           Servant.Swagger
import           Data.Swagger
import           Control.Lens

data ApplicationApi route = 
     ApplicationApi 
     { http :: route :- ToServant HttpApi AsApi
     , socket :: route :- "auth" :> ToServant WebsocketApi AsApi 
     } deriving stock Generic

newtype HttpApi route = HttpApi { root :: route :- Get '[JSON] String } 
  deriving stock Generic 

newtype WebsocketApi route = WebsocketApi { auth :: route :- ToServant AuthApi AsApi } 
  deriving stock Generic 

data AuthApi route = 
     AuthApi 
     { register 
       :: route
       :- "register" 
       :> WebSocketPending
     , authenticate
       :: route
       :- "authenticate" 
       :> WebSocketPending
     } deriving stock Generic

api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)

swaggerHttpApi :: Int -> Swagger
swaggerHttpApi port = toSwagger (genericApi (Proxy :: Proxy HttpApi)) & schemes ?~ [Http] & host ?~ Host "localhost" (Just (fromIntegral port))