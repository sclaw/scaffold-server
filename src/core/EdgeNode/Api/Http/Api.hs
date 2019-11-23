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
       , module EdgeNode.Api.Http.Search
       ) where

import Auth
import EdgeNode.Api.Http.Auth
import EdgeNode.Api.Http.User
import EdgeNode.Api.Http.Service
import EdgeNode.Api.Http.Search

import Servant.Ip
import Servant.API.Generic
import Servant.API
import Servant.Auth.Server

data HttpApi route = 
     HttpApi 
     { _httpApiAuth
       :: route 
       :- "auth"
       :> HeaderIP 
       :> ToServant AuthApi AsApi
     , _httpApiUser
       :: route
       :- "user"
       :> HeaderIP
       :> Auth '[AppJwt] JWTUser
       :> ToServant UserApi AsApi
     , _httpApiService
       :: route
       :- "service"
       :> HeaderIP
       :> Auth '[AppJwt] JWTUser
       :> ToServant ServiceApi AsApi
     , _httpApiSearch
       :: route 
       :- "search"
       :> HeaderIP
       :> Auth '[AppJwt] JWTUser
       :> ToServant SearchApi AsApi
     } deriving stock Generic 
