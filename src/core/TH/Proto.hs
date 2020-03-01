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
import EdgeNode.Transport.Qualification

import TH.Mk
import GHC.Generics
import Control.Lens
import Test.QuickCheck.Extended
import Database.Transaction

mkEnumConvertor ''Language
mkEnumConvertor ''Country
mkEnumConvertor ''Bucket
mkEnumConvertor ''QualificationCategory
mkEnumConvertor ''QualificationDegree
mkEnumConvertor ''StudyTime
mkEnumConvertor ''AcademicArea

mkSRGEqEnum ''AcademicArea "EdgeNode"
mkFromHttpApiDataEnum ''EdgeNodeAcademicArea
mkParamSchemaEnum ''EdgeNodeAcademicArea
mkSRGEqEnum ''QualificationDegree "EdgeNode"
mkFromHttpApiDataEnum ''EdgeNodeQualificationDegree
mkParamSchemaEnum ''EdgeNodeQualificationDegree
mkSRGEqEnum ''Country "EdgeNode"
mkEnumConvertor ''EdgeNodeCountry
mkParamSchemaEnum ''EdgeNodeCountry
mkFromHttpApiDataEnum ''EdgeNodeCountry

mkEnumConvertor ''EdgeNodeAcademicArea
mkToSchemaAndJSON ''EdgeNodeCountry
mkToSchemaAndJSON ''EdgeNodeQualificationDegree

mkSRGEqEnum ''Bucket "EdgeNode"
mkEnumConvertor ''EdgeNodeBucket
mkParamSchemaEnum ''EdgeNodeBucket
mkFromHttpApiDataEnum ''EdgeNodeBucket

mkEnumConvertor ''Transport.Auth.Error

mkArbitrary ''EdgeNodeAcademicArea

instance Arbitrary EdgeNodeBucket where
  arbitrary = oneof [pure Default, pure Provider]

instance ParamsShow EdgeNodeBucket where render x = x^.isoEdgeNodeBucket