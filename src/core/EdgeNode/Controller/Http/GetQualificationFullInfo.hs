{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Controller.Http.GetQualificationFullInfo (controller) where

import EdgeNode.Error
import EdgeNode.Api.Http.User.GetQualificationFullInfo
import EdgeNode.User.Qualification
import EdgeNode.Model.User
import EdgeNode.Model.Provider
import EdgeNode.Model.Qualification
import EdgeNode.Model.Category
import EdgeNode.Category
import EdgeNode.Provider.Qualification
import qualified EdgeNode.Iso as Iso
import EdgeNode.Model.User.Qualification ()

import Proto
import Katip
import KatipController
import Json
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
import Data.Aeson
import Protobuf.Scalar
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import Data.Char

controller ::  Maybe UserQualificationId -> UserId -> KatipController (Alternative (Error T.Text) GetQualificationFullInfoResponse)
controller qualId userId = 
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
            "categoryType",
            (select (case when "categoryType" = 'state_exam' then 
              (select row(s.id, json_build_object('title', s."stateExamTitle", 'country', p."providerCountry")::jsonb) 
               from "edgeNode"."StateExam" as s
               left join "edgeNode"."StateExamProvider" as sp
               on s.id = sp.state_exam_id
               left join "edgeNode"."Provider" as p
               on sp.provider_id = p.id
               where s.id = "categoryKey")
              when "categoryType" = 'higher_degree' then  
              (select row(h.id, json_build_object('country', p."providerCountry", 
              'provider', "higherDegreeProvider")::jsonb) 
               from "edgeNode"."HigherDegree" as h
               left join "edgeNode"."HigherDegreeProvider" as hp
               on h.id = hp.higher_degree_id
               left join "edgeNode"."Provider" as p
               on hp.provider_id = p.id
               where h.id = "categoryKey")
              when "categoryType" = 'international_diploma' then
              (select row(id, json_build_object('title', "internationalDiplomaTitle")::jsonb) 
               from "edgeNode"."InternationalDiploma" 
               where id = "categoryKey")
              else (select row(id, json_build_object('standard', 
              "languageStandardStandard", 'grade', "languageStandardGrade")::jsonb) 
              from "edgeNode"."LanguageStandard" 
              where id = "categoryKey") end )),
   
            (select   
               row(p.id, "providerTitle", "providerCountry", 
                   array_agg(distinct "qualificationProviderDegreeType")) 
             from "edgeNode"."Provider" as p
             left join "edgeNode"."QualificationProvider" as qp
             on p.id = qp."qualificationProviderKey"
             where p.id = uq."providerKey"
             group by p.id, "providerTitle", "providerCountry"),
           
            (select row(id, "qualificationProviderDegreeType", 
             "qualificationProviderTitle", 
             "qualificationProviderGradeRange") 
             from "edgeNode"."QualificationProvider"
             where id = uq."qualificationKey"),
           
            "qualificationSkillLevel" 
            from "edgeNode"."UserQualification" as uq 
            where (case when ($2 :: bigint) is not null 
                        then uq."userId" = $1 and uq.id = ($2 :: bigint) 
                        else uq."userId" = $1 end)|]
    let encoder = 
         (userId^._Wrapped' >$ HE.param (HE.nonNullable HE.int8)) <>
         (qualId^?_Just._Wrapped' >$ HE.param (HE.nullable HE.int8))
    let decoder = HD.rowList fullInfoDecoder 
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)

fullInfoDecoder :: HD.Row XUserQualificationFullinfo
fullInfoDecoder =
  do
    id <- fmap UserQualificationId (HD.column (HD.nonNullable HD.int8))
    categoryType <- HD.column (HD.nonNullable HD.text)
    category <- HD.column $ HD.nonNullable $ HD.composite (categoryComp categoryType)
    provider <- HD.column $ HD.nullable $ HD.composite providerComp
    qualification <- HD.column $ HD.nullable $ HD.composite qualComp
    skill <- fmap (^.from jsonb) <$> HD.column (HD.nullable HD.jsonb)
    let value = 
         XUserQualificationFullinfo 
         (Just id) 
         (Just (UserQualificationFullinfo 
                (Just category) 
                provider 
                qualification skill))
    return value
  where
    categoryComp type_ =
      do
        id <- HD.field (HD.nonNullable HD.int8)
        object <- HD.field (HD.nonNullable HD.jsonb)

        let mkUpper x = T.cons (toUpper (T.head x)) (T.tail x)
        let adjustCountry (Data.Aeson.String s) = Data.Aeson.String (T.concat (map mkUpper (T.splitOn "_" s)))  
        let modifyCountry (Object x) = Object $ HashMap.adjust adjustCountry "country" x 
        let value = 
             case type_ of 
              "state_exam" ->
                (UserQualificationFullinfoCategoryStateExam 
               . XStateExam (Just (StateExamId id)) . Just) `fmap`
                (fromJSON (modifyCountry object) :: Result StateExam)
              "higher_degree" ->  
                (UserQualificationFullinfoCategoryHigherDegree 
               . XHigherDegree (Just (HigherDegreeId id)) . Just) `fmap` 
                (fromJSON (modifyCountry object) :: Result HigherDegree)
              "international_diploma" -> 
                (UserQualificationFullinfoCategoryInternationalDiploma 
               . XInternationalDiploma (Just (InternationalDiplomaId id)) . Just) `fmap`              
                (fromJSON object :: Result InternationalDiploma)
              _ ->  
                (UserQualificationFullinfoCategoryLanguageStandard 
               . XLanguageStandard (Just (LanguageStandardId id)) . Just) `fmap` 
                (fromJSON object :: Result LanguageStandard)
        case value of 
          Success x -> return x
          Data.Aeson.Error e -> 
            error $ "category decode error: " <> e        
    providerComp = 
      do 
        id <- fmap ProviderId (HD.field (HD.nonNullable HD.int8))
        title <- HD.field (HD.nonNullable HD.text)
        country <- fmap (^.from Iso.country) (HD.field (HD.nonNullable HD.text))
        degrees <- HD.field (HD.nonNullable (HD.array (HD.dimension replicateM (HD.element (HD.nonNullable HD.text)))))
        return $ XProvider 
                 (Just id) 
                 (Just (Provider (title^.from lazytext) country)) 
                 ((degrees^..traversed.from lazytext)^.vector)
    qualComp =
      do 
        id <- fmap QualificationId (HD.field (HD.nonNullable HD.int8))
        degree <- fmap (fmap (Protobuf.Scalar.String . (^.from lazytext))) 
                  (HD.field (HD.nullable HD.text))
        title <- HD.field (HD.nonNullable HD.text)
        grade :: Result [ExGradeRange] <- fmap fromJSON (HD.field (HD.nonNullable HD.jsonb))
        let
        let mkQual = 
               XQualification (Just id) degree 
             . Just 
             . EdgeNode.Provider.Qualification.Qualification 
              (title^.from lazytext)
        case fmap (mkQual . (^.vector) . map (mkRange . exGradeRangeGrade)) grade of 
          Success x -> return x
          Data.Aeson.Error e -> 
            error $ "qualification decode error: " <> e