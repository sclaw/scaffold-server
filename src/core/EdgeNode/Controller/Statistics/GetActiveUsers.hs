{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.Statistics.GetActiveUsers (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Statement.Statistics as Statistics
import EdgeNode.Transport.Statistics
import EdgeNode.Controller.Statistics.GetRegistrations (mkGrowth)

import Katip
import KatipController
import Data.Aeson.WithField.Extended
import Data.Int
import Database.Transaction
import Control.Lens
import BuildInfo
import Data.Functor
import Data.Coerce
import qualified Data.Vector as V
import Data.String.Conv

controller :: Maybe (OnlyField "from" Int32) -> KatipController (Response Items)
controller from = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let mk tpl = (`V.snoc` EdgeNode.Transport.Statistics.Item (toS (tpl^._1)) (fromIntegral (tpl^._2)) (tpl^._3))
  response <- katipTransaction hasql $ statement Statistics.activeUsers (coerce from)
  $(logTM) DebugS $ logStr $ show $ ($ mempty) $ foldMap mk $ mkGrowth [] response
  runTelegram $location response $> (Ok . Items . ($ mempty) . foldMap mk . mkGrowth []) response