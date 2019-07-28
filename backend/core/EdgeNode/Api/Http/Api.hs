{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Api 
       ( HttpApi (..)
       , module EdgeNode.Api.Http.Auth
       , module EdgeNode.Api.Http.User
       , module EdgeNode.Api.Http.Service
       ) where

import EdgeNode.Api.Http.Auth
import EdgeNode.Api.Http.User
import EdgeNode.Api.Http.Service

import Servant.API.Generic
import Servant.API

data HttpApi route = 
     HttpApi 
     { httpApiAuth
       :: route 
       :- "auth" 
       :> ToServant AuthApi AsApi
     , httpApiUser
       :: route
       :- "user"
       :> ToServant UserApi AsApi
     , httpApiService
       :: route
       :- "service"
       :> ToServant ServiceApi AsApi                   
     } deriving stock Generic 
