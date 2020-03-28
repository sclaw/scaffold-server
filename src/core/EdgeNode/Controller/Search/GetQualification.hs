{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Search.GetQualification (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Search
import qualified EdgeNode.Statement.Search as Search

import Katip
import KatipController
import qualified Data.Text as T
import Data.Aeson.WithField.Extended
import Data.Maybe
import Data.Traversable
import Data.Coerce
import Database.Transaction
import Control.Lens

controller :: Maybe (OnlyField "query" T.Text) -> KatipController (Response [Qualification])
controller query = fmap (fromMaybe (Ok [])) $ for query (goQuery . coerce)

goQuery :: T.Text -> KatipController (Response [Qualification])
goQuery query = do 
  $(logTM) DebugS (logStr query)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap Ok $ katipTransaction hasql $ statement Search.getQualification query