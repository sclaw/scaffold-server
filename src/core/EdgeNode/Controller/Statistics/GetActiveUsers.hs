{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.Statistics.GetActiveUsers (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Statement.Statistics as Statistics
import EdgeNode.Transport.Statistics

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
  let mk tpl = (`V.snoc` Item (toS (tpl^._1)) (fromIntegral (tpl^._2)) (tpl^._3))
  let mkGrowth _ [] = []
      mkGrowth xs [x] = reverse $ (x^._1, x^._2, 0) : xs
      mkGrowth ys (x:next@(y:xs)) =
        let growth = round $ fromIntegral (y^._2 - x^._2) / (fromIntegral (x^._2)) * 100
        in mkGrowth ((x^._1, x^._2, growth):ys) next
  response <-
    fmap (Ok . Items . mempty . foldMap mk . mkGrowth []) $
    katipTransaction hasql $
    statement Statistics.activeUsers (coerce from)
  runTelegram $location response $> response