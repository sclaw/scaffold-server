{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Search.GetQualificationList (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Search
import qualified EdgeNode.Statement.Search as Search
import EdgeNode.Transport.Provider

import Katip
import KatipController
import qualified Data.Text as T
import Data.Aeson.WithField.Extended
import Data.Maybe
import Data.Traversable
import Data.Coerce
import Database.Transaction
import Control.Lens

controller :: Maybe (OnlyField "query" T.Text) -> KatipController (Response SearchQualificationList)
controller query = fmap (fromMaybe (Ok (SearchQualificationList mempty))) $ for query (goQuery . coerce)

goQuery :: T.Text -> KatipController (Response SearchQualificationList)
goQuery query = do 
  $(logTM) DebugS (logStr query)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap Ok $ katipTransaction hasql $ 
    statement Search.getQualificationList (query, ProviderCategoryHigherDegree)