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

import TH.Mk
import GHC.Generics
import Control.Lens

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