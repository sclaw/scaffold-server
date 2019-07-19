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
     --  , WebsocketApi (..)
       , api
       , swaggerHttpApi
       ) where

import qualified EdgeNode.Model.User.Entity as User
import qualified EdgeNode.Api.Http.Auth.Register as Auth

import Servant.API.Generic
import Servant.API.WebSocket ()
import Data.Proxy
import Servant.API
import Servant.Swagger
import Data.Swagger
import Control.Lens
import ReliefJsonData
import Servant.Auth.Swagger ()
import Swagger.Proto ()
import RetrofitProto
import Control.Lens.Iso.Extended

data ApplicationApi route = 
     ApplicationApi 
     { applicationApiHttp :: route :- ToServant HttpWrapperApi AsApi
   --  , applicationApiSocket :: route :- ToServant WebsocketWrapperApi AsApi 
     } deriving stock Generic

newtype HttpWrapperApi route = 
        HttpWrapperApi 
        { httpWrapperApiApi 
          :: route 
          :- "http" 
          :> ToServant HttpApi AsApi 
        } deriving stock Generic

-- newtype WebsocketWrapperApi route = 
--         WebsocketWrapperApi 
--         { websocketWrapperApiApi 
--           :: route 
--           :- "socket" 
--           :> ToServant WebsocketApi AsApi 
--         } deriving stock Generic

newtype HttpApi route = 
        HttpApi 
        { httpApiAuth
          :: route 
          :- "auth" 
          :> ToServant AuthApi AsApi 
        } deriving stock Generic

newtype AuthApi route = 
        AuthApi
        { authApiRegistration
        :: route
        :- Description "simple registration" 
        :> "registration"
        :> ReqBody '[JSON] Auth.RegisterInfo 
        :> Post '[JSON] (Alternative [Error'] User.UserIdWrapper) 
      } deriving stock Generic       

api :: Proxy (ToServantApi ApplicationApi)
api = genericApi (Proxy :: Proxy ApplicationApi)

swaggerHttpApi :: Int -> Swagger
swaggerHttpApi port = 
  toSwagger (genericApi (Proxy :: Proxy HttpWrapperApi)) 
  & schemes ?~ [Http] 
  & host ?~ Host "localhost" (Just (fromIntegral port))
  & info.description ?~ "EdgeNode server api"^.stext
  & info.version .~ "0.0.1"^.stext
  & info.contact ?~ (Contact Nothing Nothing (Just ("fclaw007@gmail.com"^.stext)))