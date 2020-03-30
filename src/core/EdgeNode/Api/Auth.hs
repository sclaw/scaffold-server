{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Auth (AuthApi (..)) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response
import EdgeNode.Model.User

import Auth
import Servant.API.Generic
import Servant.API
import Data.Aeson.WithField
import Data.Aeson.Unit
import qualified Data.Text as T

data AuthApi route = 
     AuthApi
     { _authApiAuthentication
       :: route
       :- Description "authentication"
       :> "authenticate"
       :> ReqBody '[JSON] Signin
       :> Post '[JSON]
          (Response 
           (WithId UserId 
            (WithField "role" UserRole 
             Tokens)))
     , _authApiRefreshAccessToken
       :: route 
       :- Description "refresh access token"
       :> "token"
       :> "user"
       :> Capture "uid" UserId
       :> "refresh"
       :> ReqBody '[JSON] Token
       :> Post '[JSON] (Response Tokens)
     , _authApiLogout
       :: route
       :- Description "logout by deleting refresh token"
       :> "logout"
       :> Capture "uid" UserId
       :> ReqBody '[JSON] (OnlyField "hash" T.Text)
       :> Post '[JSON] (Response Unit)
     , _authApiRegistration
       :: route
       :- Description "registration primary user"
       :> "registration"
       :> ReqBody '[JSON] Registration
       :> Post '[JSON] (Response Unit)   
     } deriving stock Generic