{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module TH.Proto where

import EdgeNode.Lang
import EdgeNode.Country
import EdgeNode.Minio
import qualified EdgeNode.Transport.Auth as Transport.Auth

import TH.Mk
import GHC.Generics
import Control.Lens
import Test.QuickCheck.Extended
import Database.Transaction

mkEnumConvertor ''Language
mkEnumConvertor ''Country
mkEnumConvertor ''Bucket

mkSRGEqEnum ''Country "EdgeNode"
mkEnumConvertor ''EdgeNodeCountry
mkParamSchemaEnum ''EdgeNodeCountry
mkFromHttpApiDataEnum ''EdgeNodeCountry

mkSRGEqEnum ''Bucket "EdgeNode"
mkEnumConvertor ''EdgeNodeBucket
mkParamSchemaEnum ''EdgeNodeBucket
mkFromHttpApiDataEnum ''EdgeNodeBucket

mkEnumConvertor ''Transport.Auth.Error

instance Arbitrary EdgeNodeBucket where
  arbitrary = oneof [pure Default, pure Provider]

instance ParamsShow EdgeNodeBucket where render x = x^.isoEdgeNodeBucket