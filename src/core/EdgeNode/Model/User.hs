{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module EdgeNode.Model.User
       ( UserRole (..)
       , RegisterStatus (..)
       , isoUserRole
       , isoRegisterStatus
       ) where

import EdgeNode.Transport.User

import TH.Mk
import qualified Data.Text.Lazy as LT
import Data.Word
import Data.Time.Transport
import Proto3.Suite.Types
import Test.QuickCheck.Extended ()
import Control.Lens.Iso.Extended
import Control.Lens
import qualified Data.Text as T
import Data.Int
import Data.Maybe
import Data.Coerce
import qualified Protobuf.Scalar as Protobuf
import Data.Generics.Product.Fields
import Data.HList.HList.Extended
import Data.Aeson.TH.Extended
import Data.Swagger
import GHC.Generics
import Data.Default.Class.Extended

mkEncoder ''Profile
mkEncoder ''FullDay

deriving instance Enum Gender

mkArbitrary ''FullDay
mkArbitrary ''Profile

data UserRole = Primary | Secondary deriving stock (Show, Generic)

data RegisterStatus = Active | Wait | TimeOut | Banned

instance ToSchema UserRole

mkEnumConvertor ''UserRole
mkEnumConvertor ''RegisterStatus

deriveJSON defaultOptions ''UserRole