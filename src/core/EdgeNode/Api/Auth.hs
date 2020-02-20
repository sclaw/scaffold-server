{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Auth (AuthApi (..)) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Model.User

import Servant.API.Generic
import Servant.API
import Data.Aeson.WithField

data AuthApi route = 
     AuthApi
     { _authApiAuthentication
       :: route
       :- Description "authentication"
       :> "authenticate"
       :> ReqBody '[JSON] SigninReq
       :> Post '[JSON]
          (Response 
           (WithId (Id "user") 
            (WithField "role" UserRole 
             Tokens)))
     , _authApiRefreshAccessToken
       :: route 
       :- Description "refresh access token"
       :> "token"
       :> "user"
       :> Capture "uid" (Id "user")
       :> "refresh"
       :> ReqBody '[JSON] Token
       :> Post '[JSON] (Response Tokens)
     } deriving stock Generic