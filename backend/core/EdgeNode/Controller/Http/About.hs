{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module EdgeNode.Controller.Http.About (controller) where

import Katip
import KatipController
import Pretty
import ReliefJsonData
import Data.Aeson.Unit
import Control.Lens.Iso.Extended
import Control.Lens

controller :: KatipController (Alternative Unit Unit)
controller = undefined