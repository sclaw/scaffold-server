{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Search.GetBarItems (controller) where

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

controller :: Maybe (OnlyField "query" T.Text) -> KatipController (Response SearchBar)
controller query = fmap (fromMaybe (Ok (SearchBar mempty))) $ for query (goQuery . coerce)

goQuery :: T.Text -> KatipController (Response SearchBar)
goQuery query = do
  $(logTM) DebugS (logStr query)
  runTelegram query
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap Ok $ katipTransaction hasql $
    statement Search.getBarItems query