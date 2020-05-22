{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.Statistics.GetActiveUsers (controller, go) where

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
import Hasql.Statement
import qualified Data.Text as T

controller :: Maybe (OnlyField "from" Int32) -> KatipController (Response Items)
controller = go Statistics.activeUsers

go :: Statement (Maybe Int32) [(T.Text, Int32)] -> Maybe (OnlyField "from" Int32) -> KatipController (Response Items)
go st from = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let mk tpl = (`V.snoc` Items_Value (toS (tpl^._1)) (fromIntegral (tpl^._2)))
  response <- katipTransaction hasql $ statement st (coerce from)
  $(logTM) DebugS $ logStr $ show $ ($ mempty) $ foldMap mk response
  runTelegram $location response $> (Ok . Items . ($ mempty) . foldMap mk) response