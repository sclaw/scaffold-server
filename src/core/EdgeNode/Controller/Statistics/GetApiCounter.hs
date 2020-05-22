{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.Statistics.GetApiCounter (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Statement.Statistics as Statistics
import EdgeNode.Transport.Statistics
import EdgeNode.Controller.Statistics.GetActiveUsers (go)

import KatipController
import Data.Aeson.WithField.Extended
import Data.Int

controller :: Maybe (OnlyField "from" Int32) -> KatipController (Response Items)
controller = go Statistics.apiCounter