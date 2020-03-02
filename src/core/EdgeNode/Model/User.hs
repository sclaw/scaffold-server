{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module EdgeNode.Model.User 
       ( User
       , UserRole (..)
       , RegisterStatus (..)
       , isoUserRole
       , isoRegisterStatus
       , encoder
       ) where

import EdgeNode.User

import TH.Mk
import qualified Data.Text.Lazy as LT
import Data.Word
import Data.Time
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

mkEncoder ''User
mkEncoder ''FullDay

instance Default Gender where def = toEnum 0
instance Default User
instance Default FullDay

mkArbitrary ''FullDay
mkArbitrary ''User 

data UserRole = Primary | Secondary deriving stock (Show, Generic)

data RegisterStatus = Active | Wait | TimeOut | Banned

instance ToSchema UserRole

mkEnumConvertor ''UserRole
mkEnumConvertor ''RegisterStatus

encoder :: User -> (T.Text, T.Text, T.Text, Int32, Int32, Int32, T.Text, Maybe Int64, Int32)
encoder x = (hToTuple . hConcat . hFromTuple) tpl
  where 
    tpl = 
      mkEncoderUser x
      & _1 %~ (hEnd . hBuild . (^.lazytext))
      & _2 %~ (hEnd . hBuild . (^.lazytext))
      & _3 %~ (hEnd . hBuild . (^.lazytext))
      & _4 %~ (hFromTuple . (& each %~ fromIntegral @_ @Int32) . mkEncoderFullDay . fromMaybe def)
      & _5 %~ (hEnd . hBuild . (^.lazytext))
      & _6 %~ (hEnd . hBuild . (^? _Just.field @"uint64Value".(integral @_ @Int64)))
      & _7 %~ (hEnd . hBuild . (either (const (0 :: Int32)) (fromIntegral . fromEnum)  . coerce @_ @(Either Int Gender)))

deriveJSON defaultOptions ''UserRole