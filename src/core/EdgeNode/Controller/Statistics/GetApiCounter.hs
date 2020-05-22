{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.Statistics.GetApiCounter (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Statement.Statistics as Statistics
import EdgeNode.Transport.Statistics

import Katip
import KatipController
import Data.Aeson.WithField.Extended
import Data.Int
import qualified Data.Vector as V
import Control.Lens
import BuildInfo
import Data.Coerce
import Database.Transaction

controller :: Maybe (OnlyField "from" Int32) -> KatipController (Response ApiCounter)
controller from = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- katipTransaction hasql $ statement Statistics.apiCounter (coerce from)
  $(logTM) DebugS $ logStr $ show response
  runTelegram $location response
  pure $ fmap (ApiCounter . V.fromList) $ fromEither response
