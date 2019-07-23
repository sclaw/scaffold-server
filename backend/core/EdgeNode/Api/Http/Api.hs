{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Api 
       ( HttpApi (..)
       , module EdgeNode.Api.Http.Auth
       ) where

import EdgeNode.Api.Http.Auth

import Servant.API.Generic
import Servant.API.WebSocket ()
import Servant.API
import Servant.Auth.Swagger ()
import Swagger.ToSchema ()


newtype HttpApi route = 
        HttpApi 
        { httpApiAuth
            :: route 
            :- "auth" 
            :> ToServant AuthApi AsApi 
        } deriving stock Generic 
