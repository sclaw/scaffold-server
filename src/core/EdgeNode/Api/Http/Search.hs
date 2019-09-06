{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Search (SearchApi (..)) where

import EdgeNode.Search
import EdgeNode.Provider.Qualification

import Servant.API.Generic
import Servant.API
import Json
import qualified Data.Text as T

data SearchApi route = 
     SearchApi
     { _searchApiSearch
       :: route
       :- Description "search"
       :> Capture "piece" SearchPiece
       :> QueryParam "query" T.Text
       :> Get '[JSON] 
          (Alternative 
           (Error T.Text) 
           XQualificationFullInfo)
     , _searchApiInit
       :: route
       :- Description "init"
       :> Get '[JSON] 
          (Alternative 
           (Error T.Text) 
           [XQualificationFullInfo])
     } deriving stock Generic