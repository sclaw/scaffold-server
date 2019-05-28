{-# LANGUAGE TemplateHaskell #-}

module Controller.Root.Root (controller) where

import Data.Functor (($>)) 
import Katip
import Pretty
import KatipHandler

controller :: KatipHandler String
controller = $(logTM) InfoS (logStr (mkPretty "debug info: " "root")) $> "welcome to main page!!"
