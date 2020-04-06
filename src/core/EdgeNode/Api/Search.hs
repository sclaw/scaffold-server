{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Search (SearchApi (..)) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Search
import EdgeNode.Transport.Id

import Auth
import Servant.API.Generic
import Servant.API
import qualified Data.Text as T
import Data.Aeson.WithField.Extended
import Servant.Auth.Server

data SearchApi route =
     SearchApi
     { _searchApiGetSearchBar
       :: route
       :- Description ""
       :> "bar"
       :> QueryParam "query"
          (OnlyField "query" T.Text)
       :> Get '[JSON] (Response SearchBar)
     , _searchApiGetQualificationList
       :: route
       :- Description ""
       :> "qualification"
       :> Auth '[AppJwt] JWTUser
       :> QueryParam "query"
          (OnlyField "query" T.Text)
       :> Get '[JSON] (Response SearchQualificationList)
     , _searchApiGetQualificationModal
       :: route
       :- Description ""
       :> "qualification"
       :> Capture "qualification_id" (Id "qualification")
       :> "modal"
       :> Get '[JSON] (Response (WithField "image" (Maybe (Id "file")) SearchQualificationModal))
     } deriving stock Generic