{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Statistics.GetActiveUsers (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Statement.Statistics as Statistics

import KatipController
import qualified Data.Text as T
import Data.Aeson.WithField.Extended
import Data.Int
import Database.Transaction
import Control.Lens
import BuildInfo
import Data.Functor
import Data.Coerce

controller :: Maybe (OnlyField "from" Int32) -> KatipController (Response [WithField "day" T.Text (OnlyField "count" Int32)])
controller from = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let mk tpl = WithField (tpl^._1) $ OnlyField (tpl^._2)
  response <- fmap (Ok . map mk) $ katipTransaction hasql $
    statement Statistics.activeUsers (coerce from)
  runTelegram $location response $> response
