{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Auth (AuthApi (..)) where

import Auth

import Servant.API.Generic
import Servant.API.WebSocket ()
import Servant.API
import Json
import Proto
import Data.Aeson.Unit
import Servant.Auth.Server    
import qualified Data.Text as T

data AuthApi route = 
     AuthApi
     { _authApiRegistration
       :: route
       :- Description "simple registration"
       :> "registration"
       :> ReqBody '[JSON] RegisterRequest 
       :> Post '[JSON] (Alternative (Error [RegisterError]) RegisterResponse)
     , _authApiSignIn
       :: route 
       :- Description "signin"
       :> "signin"
       :> ReqBody '[JSON] SignInRequest
       :> Post '[JSON] (Alternative (Error SignInError) SignInResponse)
     , _authApiRefreshToken
       :: route 
       :- Description "refresh token"
       :> "refresh-token"
       :> ReqBody '[JSON] RefreshTokenRequest
       :> Post '[JSON] (Alternative (Error RefreshTokenError) RefreshTokenResponse)
     , _authApiSignOut
       :: route 
       :- Description "signout"
       :> Auth '[AppJwt] JWTUser
       :> "signout"
       :> Post '[JSON] (Alternative (Error T.Text) Unit)             
     } deriving stock Generic