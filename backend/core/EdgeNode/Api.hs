{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api 
       ( ApplicationApi (..)
       , HttpApi (..)
       , AuthApi (..)
       , UserApi (..)
       , ServiceApi (..)
     --  , WebsocketApi (..)
       , api
       , swaggerHttpApi
       ) where

import EdgeNode.Api.Http.Api

import Servant.API.Generic
import Servant.API.WebSocket ()
import Data.Proxy
import Servant.Swagger
import Data.Swagger
import Control.Lens
import Servant.Auth.Swagger ()
import Swagger.ToSchema ()
import Control.Lens.Iso.Extended

data ApplicationApi route = 
     ApplicationApi 
     { applicationApiHttp 
       :: route 
       :- ToServant HttpWrapperApi AsApi 
     } deriving stock Generic

newtype HttpWrapperApi route = 
        HttpWrapperApi 
        { httpWrapperApiApi 
          :: route 
          :- ToServant HttpApi AsApi 
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