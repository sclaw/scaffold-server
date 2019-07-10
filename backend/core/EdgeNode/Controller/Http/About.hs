{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Http.About (controller) where

import Katip
import KatipController
import Pretty
import ReliefJsonData
import Data.Aeson.Unit
import Text.ProtocolBuffers.Basic
import Control.Lens.Iso.Extended
import Control.Lens

controller :: Seq Int -> KatipController (Alternative Unit Unit)
controller seq = const (Fortune Unit) `fmap` $(logTM) InfoS (logStr (mkPretty "debug info: " (seq^.stringify)))