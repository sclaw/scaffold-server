{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Statistics.GetRegistrations (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Statement.Statistics as Statistics
import EdgeNode.Transport.Statistics

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
  let mk tpl = (`V.snoc` Items_Value (toS (tpl^._1)) (fromIntegral (tpl^._2)))
  response <- katipTransaction hasql $ statement Statistics.registrations (coerce from)
  $(logTM) DebugS $ logStr $ show $ ($ mempty) $ foldMap mk response
  runTelegram $location response $> (Ok . Items . ($ mempty) . foldMap mk) response