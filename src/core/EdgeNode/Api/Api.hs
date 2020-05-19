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
       , module EdgeNode.Api.Service
       , module EdgeNode.Api.Site
       , module EdgeNode.Api.Statistics
       ) where

import Auth
import EdgeNode.Api.Auth
import EdgeNode.Api.User
import EdgeNode.Api.Search
import EdgeNode.Api.File
import EdgeNode.Api.Admin
import EdgeNode.Api.Provider
import EdgeNode.Api.Feedback
import EdgeNode.Api.Service
import EdgeNode.Api.Site
import EdgeNode.Api.Statistics

import Servant.API.Generic
import Servant.API
import Servant.Auth.Server
import Servant.Swagger.Tags

data HttpApi route =
     HttpApi
     { _httpApiAuth
       :: route
       :- Tags "Auth"
       :> "auth"
       :> ToServant AuthApi AsApi
     , _httpApiUser
       :: route
       :- Tags "User"
       :> "user"
       :> Auth '[AppJwt] JWTUser
       :> ToServant UserApi AsApi
     , _httpApiSearch
       :: route
       :- Tags "Search"
       :> "search"
       :> ToServant SearchApi AsApi
     , _httpApiFile
       :: route
       :- Tags "File"
       :> "file"
       :> ToServant FileApi AsApi
     , _httpApiAdmin
       :: route
       :- Tags "Admin"
       :> "admin"
       :> Auth '[Servant.Auth.Server.BasicAuth] BasicUser
       :> ToServant AdminApi AsApi
     , _httpApiProvider
       :: route
       :- Tags "Provider"
       :> "provider"
       :> Auth '[AppJwt] JWTUser
       :> ToServant ProviderApi AsApi
     , _httpApiFeedback
       :: route
       :- Tags "Feedback"
       :> "feedback"
       :> ToServant FeedbackApi AsApi
     , _httpApiService
       :: route
       :- Tags "Service"
       :> "service"
       :> ToServant ServiceApi AsApi
     , _httpApiSite
       :: route
       :- Tags "Site"
       :> "site"
       :> ToServant SiteApi AsApi
     , _httpApiStatistics
       :: route
       :- Tags "Statistics"
       :> "statistics"
       :> Auth '[Servant.Auth.Server.BasicAuth] BasicUser
       :> ToServant StatisticsApi AsApi
     } deriving stock Generic