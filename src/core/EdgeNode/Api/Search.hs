{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Search (SearchApi (..)) where

import EdgeNode.Transport.Response

import Servant.API.Generic
import Servant.API
import qualified Data.Text as T

data SearchApi route = 
     SearchApi
     { _searchApiSearch
       :: route
       :- Description ""
       :> QueryParam "query" T.Text
       :> Get '[JSON] (Response T.Text)
     } deriving stock Generic