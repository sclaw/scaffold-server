{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Api 
       ( ApplicationApi (..)
       , HttpApi (..)
       , AuthApi (..)
       , UserApi (..)
       , SearchApi (..)
       , ServiceApi (..)
       , WebApi (..)
       , GoogleApi (..)
     --  , WebsocketApi (..)
       , api
       , swaggerHttpApi
       ) where

import EdgeNode.Api.Http.Api

import Servant.API
import Servant.API.Generic
import Servant.API.WebSocket ()
import Data.Proxy
import Servant.Swagger
import Data.Swagger
import Control.Lens
import Servant.Auth.Swagger ()
import Control.Lens.Iso.Extended

data ApplicationApi route = 
     ApplicationApi 
     { _applicationApiHttp 
       :: route 
       :- ToServant HttpWrapperApi AsApi 
     } deriving stock Generic

newtype HttpWrapperApi route = 
        HttpWrapperApi 
        { _httpWrapperApiApi 
          :: route
          :- Description "http api: "
          :> "api" 
          :> "v1"
          :> ToServant HttpApi AsApi 
        } deriving stock Generic
    
api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)

swaggerHttpApi :: String -> Int -> Swagger
swaggerHttpApi hs port = 
  toSwagger (genericApi (Proxy :: Proxy HttpWrapperApi)) 
  & schemes ?~ [Http] 
  & host ?~ Host hs (Just (fromIntegral port))
  & info.description ?~ "EdgeNode server api"^.stext
  & info.version .~ "0.0.1"^.stext
  & info.contact ?~ (Contact Nothing Nothing (Just ("fclaw007@gmail.com"^.stext)))
  & info.title .~ "EdgeNode: global bank for intellectual capital"