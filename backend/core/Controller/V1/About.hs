{-# LANGUAGE TemplateHaskell #-}

module Controller.V1.About (controller) where

import Katip
import KatipController
import Pretty

controller :: KatipController ()
controller = $(logTM) InfoS (logStr (mkPretty "debug info: " "about"))