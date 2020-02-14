{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Auth (AuthApi (..)) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response

import Servant.API.Generic
import Servant.API

data AuthApi route = 
     AuthApi
     { _authApiAuthentication
       :: route
       :- Description "provder authentication"
       :> "authenticate"
       :> ReqBody '[JSON] SigninReq
       :> Post '[JSON] (Response SigninResp)
     } deriving stock Generic