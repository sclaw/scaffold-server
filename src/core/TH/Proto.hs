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
import EdgeNode.Transport.Provider
import EdgeNode.Transport.User
import EdgeNode.Transport.Feedback

import TH.Mk
import GHC.Generics hiding (from, to)
import Control.Lens
import Test.QuickCheck.Extended
import Database.Transaction
import Control.Lens.Iso.Extended
import Data.Aeson

mkEnumConvertor ''Language
mkEnumConvertor ''Country
mkEnumConvertor ''Bucket
mkEnumConvertor ''ProviderCategory
mkEnumConvertor ''QualificationDegree
mkEnumConvertor ''StudyTime
mkEnumConvertor ''AcademicArea
mkEnumConvertor ''Currency
mkEnumConvertor ''Period
mkEnumConvertor ''Gender
mkEnumConvertor ''Reason

mkSRGEqEnum ''AcademicArea "EdgeNode"
mkSRGEqEnum ''ProviderCategory "EdgeNode"
mkEnumConvertor ''EdgeNodeProviderCategory
mkEnumConvertor ''EdgeNodeAcademicArea
mkFromHttpApiDataEnum ''EdgeNodeAcademicArea [|from stext.from isoEdgeNodeAcademicArea.to Right|]
mkParamSchemaEnum ''EdgeNodeAcademicArea [|isoEdgeNodeAcademicArea.stext.to String|]
mkSRGEqEnum ''QualificationDegree "EdgeNode"
mkEnumConvertor ''EdgeNodeQualificationDegree
mkFromHttpApiDataEnum ''EdgeNodeQualificationDegree [|from stext.from isoEdgeNodeQualificationDegree.to Right|]
mkParamSchemaEnum ''EdgeNodeQualificationDegree [|isoEdgeNodeQualificationDegree.stext.to String|]
mkSRGEqEnum ''Country "EdgeNode"
mkEnumConvertor ''EdgeNodeCountry
mkParamSchemaEnum ''EdgeNodeCountry [|isoEdgeNodeCountry.stext.to String|]
mkFromHttpApiDataEnum ''EdgeNodeCountry [|from stext.from isoEdgeNodeCountry.to Right|]

mkToSchemaAndJSON ''EdgeNodeProviderCategory
mkParamSchemaEnum ''EdgeNodeProviderCategory [|isoEdgeNodeProviderCategory.stext.to String|]
mkFromHttpApiDataEnum ''EdgeNodeProviderCategory [|from stext.from isoEdgeNodeProviderCategory.to Right|]

mkToSchemaAndJSON ''EdgeNodeCountry
mkToSchemaAndJSON ''EdgeNodeQualificationDegree

mkSRGEqEnum ''Bucket "EdgeNode"
mkEnumConvertor ''EdgeNodeBucket
mkParamSchemaEnum ''EdgeNodeBucket [|isoEdgeNodeBucket.stext.to String|]
mkFromHttpApiDataEnum ''EdgeNodeBucket [|from stext.from isoEdgeNodeBucket.to Right|]

mkEnumConvertor ''Transport.Auth.Error

mkArbitrary ''EdgeNodeAcademicArea
mkArbitrary ''EdgeNodeCountry

instance Arbitrary EdgeNodeBucket where
  arbitrary = oneof [pure Default, pure Provider]

instance ParamsShow EdgeNodeBucket where render x = x^.isoEdgeNodeBucket