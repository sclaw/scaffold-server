{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Http.SaveQualifications (controller) where

import EdgeNode.Error
import EdgeNode.Api.Http.User.SaveQualifications
import EdgeNode.User.Qualification
import EdgeNode.Category
import EdgeNode.Model.User
import EdgeNode.Model.User.Qualification ()
import qualified EdgeNode.Controller.Http.SaveTrajectory as SaveTrajectory 
import EdgeNode.Model.Qualification ()

import Proto
import Katip
import KatipController
import Json
import Data.Generics.Product
import Control.Lens
import qualified Data.Text as T
import Database.Action
import Data.Either.Unwrap
import Control.Lens.Iso.Extended
import Data.Vector.Lens
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import Data.String.Interpolate
import Data.List
import Contravariant.Extras.Contrazip
import Data.Int
import Data.Maybe
import Data.Aeson (Value, toJSON)
import Data.Validation
import Data.Traversable
import Data.Bifunctor
import Data.Foldable

controller :: SaveQualificationsRequest -> UserId  -> KatipController (Alternative (Error T.Text) SaveQualificationsResponse)
controller req userId =
  do 
    $(logTM) DebugS (logStr (show req))
    raw <- (^.katipEnv.rawDB) `fmap` ask
    resp <- flip runTryDbConnHasql raw $ const (do 
      let xsm = req^._Wrapped'.field @"requestValues".from vector
      let xs = mapMaybe mkTpl xsm
      let validator = foldr checkTypeWithGrade (Success []) xs

      for validator $ \xs -> do
        let sql = 
              [i|with real as (select 
                case when t = '#{fromType TypeStateExam}' then 
                (select id from "edgeNode"."StateExam" where id = i)
                when t = '#{fromType TypeHigherDegree}' then  
                (select id from "edgeNode"."HigherDegree" where id = i)
                when t = '#{fromType TypeInternationalDiploma}' then
                (select id from "edgeNode"."InternationalDiploma" where id = i)
                else (select id from "edgeNode"."LanguageStandard" where id = i) end as ident
                from unnest($1, $2) as u(t, i))
                insert into "edgeNode"."UserQualification"
                ("categoryType", "categoryKey", "providerKey", "qualificationKey", "qualificationSkillLevel", "userId") 
                (select x.*, $6 from unnest($1, (select array_agg(ident) from real), $3, $4, $5) as x) returning id
              |]
        let vector v = HE.param (HE.array (HE.dimension foldl' (HE.element v)))
        let nullvector v = HE.param (HE.array (HE.dimension foldl' (HE.nullableElement v)))
        let encoderXs = 
              unzip5 xs >$
              contrazip5  
              (vector HE.text) 
              (vector HE.int8) 
              (nullvector HE.int8) 
              (nullvector HE.int8)
              (vector HE.jsonb)
        let encoder = encoderXs <> (userId^._Wrapped' >$ HE.param HE.int8)     
        let decoder = HD.rowList $ HD.column HD.int8 <&> UserQualificationId
        ids <- Hasql.Session.statement () (HS.Statement sql encoder decoder False)
        for_ xs $ \x -> for_ (x^._4) $ \ident -> do
          let sql = 
               [i|select tr."qualificationKey" 
                  from "edgeNode"."Trajectory" as tr 
                  left join "edgeNode"."QualificationDependency" qd
                  on tr."qualificationKey" = qd."key"   
                  where "dependency" = $1 
                        and tr."user" = $2|]
          let encoder = 
               (ident >$ HE.param HE.int8) <> 
               (userId^._Wrapped' >$ HE.param HE.int8)
          let decoder = HD.rowList $ HD.column HD.int8 <&> (^.from _Wrapped')
          keys <- Hasql.Session.statement () (HS.Statement sql encoder decoder False)  
          for_ keys $ SaveTrajectory.action userId . Just
        return ids)
    whenLeft resp ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)
    let mkResp = SaveQualificationsResponse . Response . (^.vector)
    let mkUserErr xs = ResponseError $ T.intercalate ", " xs 
    return $ either (Error . mkErr) (bimap mkUserErr mkResp . (^.from _Validation)) resp  
    
mkTpl :: UserQualification -> Maybe (T.Text, Int64, Maybe Int64, Maybe Int64, Maybe UserQualificationSkill)
mkTpl x = 
  do
    cat <- x^?field @"userQualificationCategory"._Just
    let (catType, catId) = 
          case cat of
            UserQualificationCategoryStateExamId (StateExamId i) -> 
             (fromType TypeStateExam^.stext, i)
            UserQualificationCategoryHigherDegreeId (HigherDegreeId i) -> 
             (fromType TypeHigherDegree^.stext, i)
            UserQualificationCategoryInternationalDiplomaId (InternationalDiplomaId i) -> 
             (fromType TypeInternationalDiploma^.stext, i)
            UserQualificationCategoryLanguageStandardId (LanguageStandardId i) -> 
             (fromType TypeLanguageStandard^.stext, i)
    providerm <- x^?field @"userQualificationProviderIdent"
    let provider = fmap (^.field @"providerIdValue") providerm
    qualm <- x^?field @"userQualificationQualificationIdent"
    let qual = fmap (^.field @"qualificationIdValue") qualm
    skill <- x^?field @"userQualificationSkill"
    return (catType, catId, provider, qual, skill)

checkTypeWithGrade 
  :: (T.Text, Int64, Maybe Int64, Maybe Int64, Maybe UserQualificationSkill)
  -> Validation [T.Text] [(T.Text, Int64, Maybe Int64, Maybe Int64, Value)] 
  -> Validation [T.Text] [(T.Text, Int64, Maybe Int64, Maybe Int64, Value)]
checkTypeWithGrade x validator =
  case x^._5 of 
    Just (UserQualificationSkillQualificatonSkillLevel {}) ->
     if x^._1.from stext.from isoType /= 
        TypeLanguageStandard
     then second ((:) (x & _5 %~ toJSON)) validator
     else validator *> _Failure # [[i|mismatch type and grade, type #{x^._1}, value #{x^._5}|]]
    Just (UserQualificationSkillLanguageSkillLevel {}) ->
     if x^._1.from stext.from isoType == 
        TypeLanguageStandard 
      then second ((:) (x & _5 %~ toJSON)) validator
      else validator *> _Failure # [[i|mismatch type and grade, type #{x^._1}, value #{x^._5}|]]
    Nothing -> 
      case x^._1 of  
        "international_diploma" -> second ((:) (x & _5 %~ toJSON)) validator
        _ -> validator *> _Failure # [[i|skill not found: type #{x^._1}, ident #{show (x^._2 :: Int64)}|]]