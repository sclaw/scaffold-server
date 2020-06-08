{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Auth
       ( AuthApi (..)
       , PasswordApi (..)
       ) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response
import EdgeNode.Model.User

import Auth
import Servant.API.Generic
import Servant.API
import Data.Aeson.WithField
import Data.Aeson.Unit
import qualified Data.Text as T
import Servant.Auth.Server

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
     , _authPassword
       :: route
       :- "password"
       :> ToServant PasswordApi AsApi
     } deriving stock Generic

data PasswordApi route =
     PasswordApi
     { _passwordApiNew
       :: route
       :- Description "create new password"
       :> "new"
       :> Auth '[AppJwt] JWTUser
       :> Post '[JSON] (Response Unit)
     , _passwordApiReset
       :: route
       :- Description "reset password"
       :> "reset"
       :> ReqBody '[JSON] ResetPassword
       :> Post '[JSON] (Response Unit)
     , _passwordApiRegenerate
       :: route
       :- Description "regenerate new password"
       :> "regenerate"
       :> ReqBody '[JSON] RegeneratePassword
       :> Post '[JSON] (Response Unit)
     , _passwordApiCheckToken
       :: route
       :- Description "check if token is valid"
       :> "token"
       :> "check"
       :> ReqBody '[JSON] TokenCheck
       :> Post '[JSON] (Response Bool)
     } deriving stock Generic