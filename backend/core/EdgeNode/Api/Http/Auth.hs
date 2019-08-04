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
import Swagger.ToSchema ()
import RetrofitReqRespProto
    
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
     } deriving stock Generic