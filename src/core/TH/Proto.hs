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
import EdgeNode.Transport.Provider.Qualification
import EdgeNode.Transport.Provider
import EdgeNode.Transport.User
import EdgeNode.Transport.Feedback
import EdgeNode.Transport.Provider.Pool.Tags
import qualified EdgeNode.Mail as M

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
mkEnumConvertor ''TagsStatus
mkEnumConvertor ''TrajectoryStatus
mkEnumConvertor ''M.Status
mkEnumConvertor ''M.Type

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

mkParamSchemaEnum ''Country [|isoCountry.stext.to String|]

mkSRGEqEnum ''Language "EdgeNode"
mkEnumConvertor ''EdgeNodeLanguage
mkParamSchemaEnum ''EdgeNodeLanguage [|isoEdgeNodeLanguage.stext.to String|]
mkFromHttpApiDataEnum ''EdgeNodeLanguage [|from stext.from isoEdgeNodeLanguage.to Right|]

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