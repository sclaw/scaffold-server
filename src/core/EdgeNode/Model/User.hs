{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Model.User 
       ( User
       , Type (..)
       , RegisterStatus (..)
       , isoType
       , isoRegisterStatus
       , encoder
       ) where

import EdgeNode.User

import TH.Mk
import qualified Data.Text.Lazy as LT
import Data.Word
import Data.Time
import Proto3.Suite.Types
import Data.Derive.Default
import Data.DeriveTH
import Data.Default
import Default ()
import Test.QuickCheck.Extended
import Control.Lens.Iso.Extended
import Control.Lens
import qualified Data.Text as T
import Data.Int
import Data.Maybe
import Data.Coerce
import qualified Protobuf.Scalar as Protobuf
import Protobuf.Scalar (UInt64 (..)) 
import Data.Generics.Product.Fields
import Data.HList.HList.Extended

mkEncoder ''User
mkEncoder ''FullDay
derive makeDefault ''Gender
derive makeDefault ''User
derive makeDefault ''FullDay
derive makeArbitrary ''FullDay
derive makeArbitrary ''User
derive makeArbitrary ''UInt64

data Type = Primary | Secondary 

data RegisterStatus = Active | Wait | TimeOut | Banned

mkEnumConvertor ''Type
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