{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Statistics.GetRegistrations (controller, mkGrowth) where

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
import qualified Data.Text as T

controller :: Maybe (OnlyField "from" Int32) -> KatipController (Response Items)
controller from = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let mk tpl = (`V.snoc` EdgeNode.Transport.Statistics.Item (toS (tpl^._1)) (fromIntegral (tpl^._2)) (tpl^._3))
  response <- katipTransaction hasql $ statement Statistics.registrations (coerce from)
  $(logTM) DebugS $ logStr $ show $ ($ mempty) $ foldMap mk $ mkGrowth [] response
  runTelegram $location response $> (Ok . Items . ($ mempty) . foldMap mk . mkGrowth []) response

mkGrowth :: [(T.Text, Int32, Int32)] -> [(T.Text, Int32)] -> [(T.Text, Int32, Int32)]
mkGrowth xs [] = reverse xs
mkGrowth ys (x:xs) = mkGrowth ((x^._1, x^._2, 0):ys) xs