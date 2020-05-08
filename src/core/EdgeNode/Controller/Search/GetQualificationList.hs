{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Search.GetQualificationList
       ( unauthorizedController
       , authorizedController
       ) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Search
import qualified EdgeNode.Statement.Search as Search
import EdgeNode.Transport.Provider

import Auth
import Katip
import KatipController
import qualified Data.Text as T
import Data.Aeson.WithField.Extended
import Data.Maybe
import Data.Traversable
import Data.Coerce
import Database.Transaction
import Control.Lens
import Data.Functor
import BuildInfo

unauthorizedController :: Maybe (OnlyField "query" T.Text) -> KatipController (Response SearchQualificationList)
unauthorizedController query = fmap (fromMaybe (Ok (SearchQualificationList mempty))) $ for query (goQuery Nothing . coerce)

authorizedController :: Maybe (OnlyField "query" T.Text) -> UserId -> KatipController (Response SearchQualificationList)
authorizedController query user_id = fmap (fromMaybe (Ok (SearchQualificationList mempty))) $ for query (goQuery (Just user_id)  . coerce)

goQuery :: Maybe UserId -> T.Text -> KatipController (Response SearchQualificationList)
goQuery user_id_m query = do
  $(logTM) DebugS (logStr query)
  runTelegram $location (user_id_m, query)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap Ok $ katipTransaction hasql $
    case user_id_m of
      Just user_id -> statement Search.getAuthorizedQualificationList (query, ProviderCategoryHigherDegree, user_id)
      Nothing -> statement Search.getQualificationList (query, ProviderCategoryHigherDegree)
  runTelegram $location response $> response