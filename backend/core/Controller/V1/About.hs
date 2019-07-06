{-# LANGUAGE TemplateHaskell #-}

module Controller.V1.About (controller) where

import Katip
import KatipController
import Pretty
import ReliefJsonData

controller :: KatipController (Alternative () Unit)
controller = (Fortune . Unit) `fmap` $(logTM) InfoS (logStr (mkPretty "debug info: " "about"))