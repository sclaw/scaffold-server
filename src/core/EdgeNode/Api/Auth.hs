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
       :- Description "provder authentication"
       :> "authenticate"
       :> ReqBody '[JSON] SigninReq
       :> Post '[JSON]
          (Response 
           (WithId (Id "user") 
            (WithField "role" Type 
             SigninResp)))
     } deriving stock Generic