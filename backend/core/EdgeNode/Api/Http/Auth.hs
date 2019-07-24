{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Auth (AuthApi (..)) where

import qualified EdgeNode.Model.User.Entity as User
import qualified EdgeNode.Api.Http.Auth.Register as Reg

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
       :> ReqBody '[JSON] Reg.RegisterInfo 
       :> Post '[JSON] (Alternative [ErrorReg] User.UserIdWrapper)
     , authApiSignIn
       :: route 
       :- Description "signin"
       :> "signin"
       :> ReqBody '[JSON] SignInRequest
       :> Post '[JSON] (Alternative ErrorSignIn SignInResponse)
     , authApiRefreshToken
       :: route 
       :- Description "refresh token"
       :> "refresh-token"
       :> ReqBody '[JSON] RefreshTokenRequest
       :> Post '[JSON] (Alternative ErrorRefreshToken RefreshTokenResponse)
     } deriving stock Generic