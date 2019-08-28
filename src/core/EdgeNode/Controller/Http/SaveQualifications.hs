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

import RetrofitProto
import Katip
import KatipController
import ReliefJsonData
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
import Data.Typeable
import Data.Maybe
import Data.Aeson (Value, toJSON)
import Data.Validation
import Data.Traversable
import Data.Bifunctor

controller :: UserId -> SaveQualificationsRequest -> KatipController (Alternative (Error T.Text) SaveQualificationsResponse)
controller userId req =
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
                (select id from "edgeNode"."#{show (typeOf (undefined :: StateExam))}" where id = i)
                when t = '#{fromType TypeHigherDegree}' then  
                (select id from "edgeNode"."#{show (typeOf (undefined :: HigherDegree))}" where id = i)
                when t = '#{fromType TypeInternationalDiploma}' then
                (select id from "edgeNode"."#{show (typeOf (undefined :: InternationalDiploma))}" where id = i)
                else (select id from "edgeNode"."#{show (typeOf (undefined :: LanguageStandard))}" where id = i) end as ident
                from unnest($1, $2) as u(t, i))
                insert into "edgeNode"."#{show (typeOf (undefined :: UserQualification))}"
                ("categoryType", "categoryKey", "providerKey", "qualificationKey", "qualificationSkillLevel", "userId") 
                (select x.*, $6 from unnest($1, (select array_agg(ident) from real), $3, $4, $5) as x) returning id
              |]
        let vector v = HE.param (HE.array (HE.dimension foldl' (HE.element v)))
        let encoderXs = 
              unzip5 xs >$
              contrazip5 
              (vector HE.text) 
              (vector HE.int8) 
              (vector HE.int8) 
              (vector HE.int8)
              (vector HE.jsonb)
        let encoder = encoderXs <> (userId^._Wrapped' >$ HE.param HE.int8)     
        let decoder = HD.rowList $ HD.column HD.int8 <&> UserQualificationId
        Hasql.Session.statement () (HS.Statement sql encoder decoder False)) 
    whenLeft resp ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)
    let mkResp = SaveQualificationsResponse . Response . (^.vector)
    let mkUserErr xs = ResponseError $ T.intercalate ", " xs 
    return $ either (Error . mkErr) (bimap mkUserErr mkResp . (^.from _Validation)) resp  
    
mkTpl :: UserQualification -> Maybe (T.Text, Int64, Int64, Int64, UserQualificationSkill)
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
    provider <- x^?field @"userQualificationProviderIdent"._Just.field @"providerIdValue"
    qual <- x^?field @"userQualificationQualificationIdent"._Just.field @"qualificationIdValue"
    skill <- x^?field @"userQualificationSkill"._Just
    return (catType, catId, provider, qual, skill)

checkTypeWithGrade 
  :: (T.Text, Int64, Int64, Int64, UserQualificationSkill)
  -> Validation [T.Text] [(T.Text, Int64, Int64, Int64, Value)] 
  -> Validation [T.Text] [(T.Text, Int64, Int64, Int64, Value)]
checkTypeWithGrade x validator =
  case x^._5 of 
    UserQualificationSkillQualificatonSkillLevel {} ->
     if x^._1.from stext.from isoType /= 
        TypeLanguageStandard
     then second ((:) (x & _5 %~ toJSON)) validator
     else validator *> _Failure # [[i|mismatch type and grade, type #{x^._1}, value #{x^._5}|]]
    UserQualificationSkillLanguageSkillLevel {} ->
     if x^._1.from stext.from isoType == 
        TypeLanguageStandard 
      then second ((:) (x & _5 %~ toJSON)) validator
      else validator *> _Failure # [[i|mismatch type and grade, type #{x^._1}, value #{x^._5}|]]