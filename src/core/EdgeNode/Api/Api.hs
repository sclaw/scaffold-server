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
       , module EdgeNode.Api.Admin 
       , module EdgeNode.Api.Provider
       ) where

import Auth
import EdgeNode.Api.Auth
import EdgeNode.Api.User
import EdgeNode.Api.Service
import EdgeNode.Api.Search
import EdgeNode.Api.File
import EdgeNode.Api.Admin
import EdgeNode.Api.Provider

import Servant.Ip
import Servant.API.Generic
import Servant.API
import Servant.Auth.Server
import Servant.Swagger.Tags

data HttpApi route = 
     HttpApi 
     { _httpApiAuth
       :: route
       :- Tags "auth" 
       :> "auth"
       :> HeaderIP 
       :> ToServant AuthApi AsApi
     , _httpApiUser
       :: route
       :- Tags "user"
       :> "user"
       :> HeaderIP
       :> Auth '[AppJwt] JWTUser
       :> ToServant UserApi AsApi
     , _httpApiService
       :: route
       :- Tags "service"
       :> "service"
       :> HeaderIP
       :> Auth '[AppJwt] JWTUser
       :> ToServant ServiceApi AsApi
     , _httpApiSearch
       :: route 
       :- Tags "search"
       :> "search"
       :> HeaderIP
       :> ToServant SearchApi AsApi
     , _httpApiFile  
       :: route 
       :- Tags "file"
       :> "file"
       :> HeaderIP
       :> ToServant FileApi AsApi
     , _httpApiAdmin
       :: route
       :- Tags "admin" 
       :> "admin"
       :> HeaderIP
       :> Auth '[Servant.Auth.Server.BasicAuth] BasicUser
       :> ToServant AdminApi AsApi
     , _httpApiProvider
       :: route 
       :- Tags "provider"
       :> "provider"
       :> HeaderIP
       :> Auth '[AppJwt] JWTUser
       :> ToServant ProviderApi AsApi              
     } deriving stock Generic 
