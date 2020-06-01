{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Provider.PublishTags (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider

import Auth
import KatipController
import Data.Aeson.Unit
import Database.Transaction
import Control.Lens
import BuildInfo
import Data.Traversable

controller :: Id "tags" -> UserId -> KatipController (Response Unit)
controller tags_id user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  runTelegram $location (tags_id, user_id)
  fmap (const (Ok Unit)) $ katipTransaction hasql $ do
    (qual_id, xs) <- statement Provider.getTagsValues tags_id
    us <- for xs $ statement Provider.getMatchedUsers
    statement Provider.savePromotedQualification (qual_id, us)