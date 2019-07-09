{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Http.About (controller) where

import Katip
import KatipController
import Pretty
import ReliefJsonData
import Data.Aeson.Unit

controller :: KatipController (Alternative Unit Unit)
controller = const (Fortune Unit) `fmap` $(logTM) InfoS (logStr (mkPretty "debug info: " "about"))