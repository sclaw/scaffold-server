{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Auth (AuthApi (..)) where

import Servant.API.Generic
import Servant.API.WebSocket ()
import Servant.API
import ReliefJsonData
import Servant.Auth.Swagger ()
import Swagger.ToSchema ()
import RetrofitProto
    
data AuthApi route = 
     AuthApi
     { authApiRegistration
       :: route
       :- Description "simple registration"
       :> "registration"
       :> ReqBody '[JSON] RegisterRequest 
       :> Post '[JSON] (Alternative RegisterError RegisterResponse)
     , authApiSignIn
       :: route 
       :- Description "signin"
       :> "signin"
       :> ReqBody '[JSON] SignInRequest
       :> Post '[JSON] (Alternative SignInError SignInResponse)
     , authApiRefreshToken
       :: route 
       :- Description "refresh token"
       :> "refresh-token"
       :> ReqBody '[JSON] RefreshTokenRequest
       :> Post '[JSON] (Alternative RefreshTokenError RefreshTokenResponse)
     } deriving stock Generic