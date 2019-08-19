{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Search (SearchApi (..)) where

import EdgeNode.Search

import Servant.API.Generic
import Servant.API
import ReliefJsonData
import RetrofitProto
import qualified Data.Text as T

newtype SearchApi route = 
        SearchApi
        { _searchApiSearch
          :: route
          :- Description "search"
          :> Capture "piece" SearchPiece
          :> ReqBody '[JSON] QualificationRequest
          :> Post '[JSON] 
            (Alternative 
             (Error T.Text) 
             QualificationResponse)     
        } deriving stock Generic