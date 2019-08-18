{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.GetQualificationFullInfo (controller) where

import EdgeNode.Error
import EdgeNode.Api.Http.User.GetQualificationFullInfo
import EdgeNode.User.Qualification
import EdgeNode.Model.User
import EdgeNode.Model.Provider
import EdgeNode.Model.Qualification
import EdgeNode.Model.Category
import EdgeNode.Category
import EdgeNode.Qualification
import qualified EdgeNode.Iso as Iso
import EdgeNode.Model.User.Qualification ()

import RetrofitProto
import Katip
import KatipController
import ReliefJsonData
import Database.Action
import qualified Data.Text as T
import Data.Either.Unwrap
import Control.Lens.Iso.Extended
import Control.Lens
import Data.Vector.Lens
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.String.Interpolate
import Data.Typeable
import Data.Aeson
import Control.Applicative ((<|>))
import Protobuf.Scalar

controller :: UserId -> Maybe UserQualificationId -> KatipController (Alternative (Error T.Text) GetQualificationFullInfoResponse)
controller userId qualId = 
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql (action userId qualId) raw
    whenLeft x ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)
    let mkResp = GetQualificationFullInfoResponse . Response . (^.vector)     
    return $ bimap mkErr mkResp x^.eitherToAlt

action :: UserId -> Maybe UserQualificationId -> KatipLoggerIO -> Hasql.Session.Session [XUserQualificationFullinfo]
action userId qualId  _ =
  do
    let sql = 
         [i|select
            id, 
            (select (case when "categoryType" = '#{fromType TypeStateExam}' then 
              (select row(id, json_build_object('title', "stateExamTitle", 'country', "stateExamCountry")::jsonb) 
               from "edgeNode"."#{show (typeOf (undefined :: StateExam))}" 
               where id = "categoryKey")
              when "categoryType" = '#{fromType TypeHigherDegree}' then  
              (select row(id, json_build_object('country', "higherDegreeCountry", 
              'provider', "higherDegreeProvider")::jsonb) 
               from "edgeNode"."#{show (typeOf (undefined :: HigherDegree))}" 
               where id = "categoryKey")
              when "categoryType" = '#{fromType TypeInternationalDiploma}' then
              (select row(id, json_build_object('title', "internationalDiplomaTitle")::jsonb) 
               from "edgeNode"."#{show (typeOf (undefined :: InternationalDiploma))}" 
               where id = "categoryKey")
              else (select row(id, json_build_object('standard', 
              "languageStandardStandard", 'grade', "languageStandardGrade")::jsonb) 
              from "edgeNode"."#{show (typeOf (undefined :: LanguageStandard))}" 
              where id = "categoryKey") end )), 
            (select row(id, "providerTitle", "providerCountry") 
             from "edgeNode"."#{show (typeOf (undefined :: Provider))}"
             where id = uq."providerKey"),
            (select row(id, "qualificationProviderDegreeType", 
             "qualificationProviderTitle", 
             "qualificationProviderGrade") 
             from "edgeNode"."#{show (typeOf (undefined :: QualificationProvider))}"
             where id = uq."providerKey")   
            from "edgeNode"."#{show (typeOf (undefined :: UserQualification))}" as uq 
              where "userId" = $1 and (case when ($2 :: bigint) is not null then uq.id = ($2 :: bigint) else true end) |]
    let encoder = 
         (userId^._Wrapped' >$ HE.param HE.int8) <>
         (qualId^?_Just._Wrapped' >$ HE.nullableParam HE.int8)
    let decoder = HD.rowList fullInfoDecoder 
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)

fullInfoDecoder :: HD.Row XUserQualificationFullinfo
fullInfoDecoder = 
  do
    id <- fmap UserQualificationId (HD.column HD.int8)
    category <- HD.column $ HD.composite categoryComp
    provider <- HD.nullableColumn $ HD.composite providerComp
    qualification <- HD.nullableColumn $ HD.composite qualComp
    let value = 
         XUserQualificationFullinfo 
         (Just id) 
         (Just (UserQualificationFullinfo 
                (Just category) 
                provider 
                qualification))
    return value
  where
    categoryComp =
      do
        id <- HD.field HD.int8
        json <- HD.field HD.jsonb
        let value = 
             ((UserQualificationFullinfoCategoryStateExam 
             . XStateExam (Just (StateExamId id)) . Just) `fmap` 
              (fromJSON json :: Result StateExam)) <|> 
             ((UserQualificationFullinfoCategoryHigherDegree 
             . XHigherDegree (Just (HigherDegreeId id)) . Just) `fmap` 
              (fromJSON json :: Result HigherDegree)) <|> 
             ((UserQualificationFullinfoCategoryInternationalDiploma 
             . XInternationalDiploma (Just (InternationalDiplomaId id)) . Just) `fmap`              
              (fromJSON json :: Result InternationalDiploma)) <|>
             ((UserQualificationFullinfoCategoryLanguageStandard 
             . XLanguageStandard (Just (LanguageStandardId id)) . Just) `fmap` 
              (fromJSON json :: Result LanguageStandard))
        case value of 
          Success x -> return x
          Data.Aeson.Error e -> 
            error $ "category decode error: " <> e        
    providerComp = 
      do 
        id <- fmap ProviderId (HD.field HD.int8)
        title <- HD.field HD.text
        country <- fmap (^.from Iso.country) (HD.field HD.text)
        return $ XProvider (Just id) (Just (Provider (title^.from lazytext) country))
    qualComp =
      do 
        id <- fmap QualificationId (HD.field HD.int8)
        degree <- fmap (fmap (Protobuf.Scalar.String . (^.from lazytext))) 
                  (HD.nullableField HD.text)
        title <- HD.field HD.text
        grade <-  fmap fromJSON (HD.field HD.jsonb)
        let mkQual = XQualification (Just id) degree . Just . Qualification (title^.from lazytext)
        case fmap mkQual grade of 
          Success x -> return x
          Data.Aeson.Error e -> 
            error $ "qualification decode error: " <> e