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
       , module EdgeNode.Api.Search
       , module EdgeNode.Api.File
       , module EdgeNode.Api.Admin
       , module EdgeNode.Api.Provider
       , module EdgeNode.Api.Feedback
       ) where

import Auth
import EdgeNode.Api.Auth
import EdgeNode.Api.User
import EdgeNode.Api.Search
import EdgeNode.Api.File
import EdgeNode.Api.Admin
import EdgeNode.Api.Provider
import EdgeNode.Api.Feedback

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
       :> ToServant AuthApi AsApi
     , _httpApiUser
       :: route
       :- Tags "user"
       :> "user"
       :> Auth '[AppJwt] JWTUser
       :> ToServant UserApi AsApi
     , _httpApiSearch
       :: route
       :- Tags "search"
       :> "search"
       :> ToServant SearchApi AsApi
     , _httpApiFile
       :: route
       :- Tags "file"
       :> "file"
       :> ToServant FileApi AsApi
     , _httpApiAdmin
       :: route
       :- Tags "admin"
       :> "admin"
       :> Auth '[Servant.Auth.Server.BasicAuth] BasicUser
       :> ToServant AdminApi AsApi
     , _httpApiProvider
       :: route
       :- Tags "provider"
       :> "provider"
       :> Auth '[AppJwt] JWTUser
       :> ToServant ProviderApi AsApi
     , _httpApiFeedback
       :: route
       :- Tags "feedback"
       :> "feedback"
       :> ToServant FeedbackApi AsApi
     } deriving stock Generic