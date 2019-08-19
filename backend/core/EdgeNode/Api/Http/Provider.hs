{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module EdgeNode.Api.Http.Provider (ProviderApi (..)) where

import EdgeNode.Model.Provider

import Servant.API.Generic
import Servant.API
import ReliefJsonData
import RetrofitProto
import qualified Data.Text as T

newtype ProviderApi route = 
        ProviderApi
        { _providerApiSearchQualification
          :: route
          :- Description "search qualification"
          :> "search" 
          :> Capture "piece" SearchPiece
          :> ReqBody '[JSON] SearchQualificationRequest
          :> Post '[JSON] (Alternative (Error T.Text) SearchQualificationResponse)     
        } deriving stock Generic