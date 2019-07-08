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
       , V1Api (..)
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
import           ReliefJsonData
import           Data.Aeson.Unit

data ApplicationApi route = 
     ApplicationApi 
     { applicationApiHttp :: route :- ToServant HttpApi AsApi
     , applicationApiSocket :: route :- "auth" :> ToServant WebsocketApi AsApi 
     } deriving stock Generic

data HttpApi route = 
     HttpApi 
     { httpApiRoot 
       :: route
       :- Summary "Endpoint for .."
       :> Description "" 
       :> Get '[JSON] String
     , httpApiV1
       :: route
       :- Summary "Endpoint for .."
       :> Description ""
       :> "api"
       :> "v1"
       :> ToServant V1Api AsApi 
     } deriving stock Generic 

newtype WebsocketApi route = WebsocketApi { websocketApiAuth :: route :- ToServant AuthApi AsApi } 
  deriving stock Generic 

data AuthApi route = 
     AuthApi 
     { authApiRegister 
       :: route
       :- Summary "Endpoint for .."
       :> Description ""
       :> "register" 
       :> WebSocketPending
     , authApiAuthenticate
       :: route
       :- Summary "Endpoint for .."
       :> Description ""       
       :> "authenticate" 
       :> WebSocketPending
     } deriving stock Generic

newtype V1Api route = V1Api { v1ApiAbout :: route :- "about" :> Post '[JSON] (Alternative Unit Unit) } deriving stock Generic

api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)

swaggerHttpApi :: Int -> Swagger
swaggerHttpApi port = 
  toSwagger (genericApi (Proxy :: Proxy HttpApi)) 
  & schemes ?~ [Http] & 
  host ?~ Host "localhost" (Just (fromIntegral port))