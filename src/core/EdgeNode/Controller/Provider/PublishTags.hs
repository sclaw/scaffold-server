{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Provider.PublishTags (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Transport.Provider.Pool.Tags (TagsStatus (..))

import Auth
import KatipController
import Data.Aeson.Unit
import Database.Transaction
import Control.Lens
import BuildInfo
import Data.Traversable
import Validation
import Data.Functor
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Time.Clock
import Data.Maybe
import Data.Coerce
import Control.Monad.IO.Class
import Data.String.Conv

controller :: Id "tags" -> UserId -> KatipController (Response Unit)
controller tags_id user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  runTelegram $location (tags_id, user_id)
  telegram <- fmap (^.katipEnv.telegram) ask
  fmap (fromValidation . first (map asError)) $
    katipTransaction hasql $ do
      log <- ask
      tags@Tags {..} <- statement Provider.getTags tags_id
      liftIO $ send telegram log $ toS $ "tags: " <> show tags
      for (validate tags) $ const $ do
        uss <- for tagsTags $ statement Provider.getMatchedUsers
        let us = V.concat $ V.toList uss
        liftIO $ send telegram log $ toS $ "users: " <> show us
        statement Provider.savePromotedQualification (coerce tags_id, tagsQualifiationId, us) $> Unit

data Error = ErrorTagsEmpty | ErrorFrozen UTCTime

instance Error.AsError Error where
  asError ErrorTagsEmpty = Error.asError @T.Text "tags shouldn't be empty"
  asError (ErrorFrozen tm) = Error.asError @T.Text $ "the given tags are frozen to be changed up to " <> T.pack (show tm)

validate :: Tags -> Validation [Error] ()
validate Tags {..} =
  if V.null tagsTags then Failure [ErrorTagsEmpty] else Success () *>
  case tagsStatus of
   TagsStatusNew -> Success ()
   TagsStatusPublished -> Failure [ErrorFrozen (fromMaybe (error "frozen tm empty") tagsFrozenTM)]