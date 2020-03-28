{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Search (SearchApi (..)) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Search

import Servant.API.Generic
import Servant.API
import qualified Data.Text as T
import Data.Aeson.WithField.Extended

data SearchApi route = 
     SearchApi
     { _searchApiSearchQualification
       :: route
       :- Description ""
       :> "qualification"
       :> QueryParam "query" 
          (OnlyField "query" T.Text)
       :> Get '[JSON] 
          (Response [SearchQualification])
     } deriving stock Generic