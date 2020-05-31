{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Provider.CreateTags (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider.Pool.Tags
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider

import Auth
import KatipController
import Katip
import Database.Transaction
import Control.Lens
import Pretty
import BuildInfo

controller :: TagsBuilder -> UserId -> KatipController (Response (Id "tags"))
controller builder _ = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  runTelegram $location builder
  $(logTM) DebugS (logStr ("tags: " ++ mkPretty mempty builder))
  fmap Ok $ katipTransaction hasql (statement Provider.createTags builder)