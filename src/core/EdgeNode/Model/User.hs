{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Model.User (UserEncoder, mkEncoderUser) where

import EdgeNode.User

import TH.Mk
import qualified Data.Text.Lazy as LT
import Data.Word
import Data.Time
import Proto3.Suite.Types

mkEncoder ''User