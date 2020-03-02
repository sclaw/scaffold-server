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
import GHC.Generics hiding (from, to)
import Control.Lens
import Test.QuickCheck.Extended
import Database.Transaction
import Control.Lens.Iso.Extended

mkEnumConvertor ''Language
mkEnumConvertor ''Country
mkEnumConvertor ''Bucket
mkEnumConvertor ''QualificationCategory
mkEnumConvertor ''QualificationDegree
mkEnumConvertor ''StudyTime
mkEnumConvertor ''AcademicArea

mkSRGEqEnum ''AcademicArea "EdgeNode"
mkEnumConvertor ''EdgeNodeAcademicArea
mkFromHttpApiDataEnum ''EdgeNodeAcademicArea [|from stext.from isoEdgeNodeAcademicArea.to Right|]
mkParamSchemaEnum ''EdgeNodeAcademicArea
mkSRGEqEnum ''QualificationDegree "EdgeNode"
mkEnumConvertor ''EdgeNodeQualificationDegree
mkFromHttpApiDataEnum ''EdgeNodeQualificationDegree [|from stext.from isoEdgeNodeQualificationDegree.to Right|]
mkParamSchemaEnum ''EdgeNodeQualificationDegree
mkSRGEqEnum ''Country "EdgeNode"
mkEnumConvertor ''EdgeNodeCountry
mkParamSchemaEnum ''EdgeNodeCountry
mkFromHttpApiDataEnum ''EdgeNodeCountry [|from stext.from isoEdgeNodeCountry.to Right|]

mkToSchemaAndJSON ''EdgeNodeCountry
mkToSchemaAndJSON ''EdgeNodeQualificationDegree

mkSRGEqEnum ''Bucket "EdgeNode"
mkEnumConvertor ''EdgeNodeBucket
mkParamSchemaEnum ''EdgeNodeBucket
mkFromHttpApiDataEnum ''EdgeNodeBucket [|from stext.from isoEdgeNodeBucket.to Right|]

mkEnumConvertor ''Transport.Auth.Error

mkArbitrary ''EdgeNodeAcademicArea
mkArbitrary ''EdgeNodeCountry

instance Arbitrary EdgeNodeBucket where
  arbitrary = oneof [pure Default, pure Provider]

instance ParamsShow EdgeNodeBucket where render x = x^.isoEdgeNodeBucket