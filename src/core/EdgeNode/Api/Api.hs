{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Api 
       ( HttpApi (..)
       , module EdgeNode.Api.Auth
       , module EdgeNode.Api.User
       , module EdgeNode.Api.Service
       , module EdgeNode.Api.Search
       , module EdgeNode.Api.File
       ) where

import Auth
import EdgeNode.Api.Auth
import EdgeNode.Api.User
import EdgeNode.Api.Service
import EdgeNode.Api.Search
import EdgeNode.Api.File
import EdgeNode.Api.Admin

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
     , _httpApiFile  
       :: route 
       :- "file"
       :> HeaderIP
       :> ToServant FileApi AsApi
     , _httpApiAdmin
       :: route 
       :- "admin"
       :> HeaderIP
       :> ToServant AdminApi AsApi       
     } deriving stock Generic 
