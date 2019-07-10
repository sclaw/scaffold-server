{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api 
       ( ApplicationApi (..)
       , AuthApi (..)
       , HttpApi (..)
       , WebsocketApi (..)
       , api
       , swaggerHttpApi
       ) where

import qualified EdgeNode.Auth.User as Auth

import           Servant.API.Generic
import           Servant.API.WebSocket
import           Data.Proxy
import           Servant.API
import           Servant.Swagger
import           Data.Swagger
import           Control.Lens
import           ReliefJsonData
import           Data.Aeson.Unit
import           Servant.Auth
import           Servant.Auth.Swagger ()

data ApplicationApi route = 
     ApplicationApi 
     { applicationApiHttp :: route :- ToServant HttpWrapperApi AsApi
     , applicationApiSocket :: route :- ToServant WebsocketWrapperApi AsApi 
     } deriving stock Generic

newtype HttpWrapperApi route = 
        HttpWrapperApi 
        { httpWrapperApiApi 
          :: route 
          :- "http" 
          :> ToServant HttpApi AsApi 
        } deriving stock Generic

newtype WebsocketWrapperApi route = 
        WebsocketWrapperApi 
        { websocketWrapperApiApi 
          :: route 
          :- "socket" 
          :> ToServant WebsocketApi AsApi 
        } deriving stock Generic

newtype HttpApi route = 
        HttpApi 
        { httpApiAbout 
          :: route
          :- Auth '[JWT] Auth.JWTUser 
          :> "about" 
          :> Get '[JSON] (Alternative Unit Unit) 
        } deriving stock Generic

newtype WebsocketApi route = WebsocketApi { websocketApiAuth :: route :- "auth" :> ToServant AuthApi AsApi } 
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

api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)

swaggerHttpApi :: Int -> Swagger
swaggerHttpApi port = 
  toSwagger (genericApi (Proxy :: Proxy HttpWrapperApi)) 
  & schemes ?~ [Http] & 
  host ?~ Host "localhost" (Just (fromIntegral port))