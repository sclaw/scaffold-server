{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Root.Root (controller) where

import Data.Functor (($>)) 
import Katip
import Pretty
import KatipController

controller :: KatipController String
controller = $(logTM) InfoS (logStr (mkPretty "debug info: " "root")) $> "welcome to main page!!"
