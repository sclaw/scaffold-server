{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Controller.Http.PatchQualifications (controller) where

import EdgeNode.Model.User
import EdgeNode.Error
import EdgeNode.Api.Http.User.PatchQualifications
import EdgeNode.User.Qualification
import EdgeNode.Model.User.Qualification (UserQualificationSkill (..))
import EdgeNode.Model.Qualification
import qualified EdgeNode.Controller.Http.SaveTrajectory as SaveTrajectory 

import Data.Generics.Product
import Control.Lens
import Proto
import Json
import KatipController
import Data.Aeson.Unit
import qualified Data.Text as T
import Katip
import Database.Action
import Data.Either.Unwrap
import Control.Lens.Iso.Extended
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.Traversable
import Data.Either.Combinators (maybeToRight)
import Data.String.Interpolate
import Data.Aeson
import Control.Monad.Error.Class (throwError)
import Data.Either
import Control.Monad
import Data.Foldable

controller :: PatchQualificationsRequest -> UserId -> KatipController (Alternative (Error T.Text) Unit)
controller (PatchQualificationsRequest (Request Nothing)) _ = return $ Json.Error $ ResponseError "empty request"
controller req uid =
  do
    let Just Request_Value {..} = req^._Wrapped'.field @"requestValue" 
    raw <- (^.katipEnv.rawDB) `fmap` ask
    let go = do
          e <- action uid request_ValueIdent request_ValueSkill
          traverse (\xs -> for_ xs $ SaveTrajectory.action uid . Just) e
    x <- runTryDbConnHasql   (const go) raw
    whenLeft x ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)
    case x of 
      Left e -> return $ Json.Error $ mkErr e
      Right (Left e) -> return $ Json.Error $ ResponseError e
      Right (Right _) -> return $ Fortune Unit

action :: UserId ->  Maybe UserQualificationId -> Maybe Request_ValueSkill -> Hasql.Session.Session (Either T.Text [QualificationId])
action uid qidm skillm = fmap (join . maybeToRight "qualififcation or (and) skill are absent, or both") $ for ((,) <$> qidm <*> skillm) (uncurry go)
  where
    go ident skill = 
      do
        let sqlXs = 
              [i|
                select qp."qualificationProviderGradeRange" 
                from "edgeNode"."UserQualification" as uq
                left join "edgeNode"."QualificationProvider" as qp
                on uq."qualificationKey" = qp.id
                where uq.id = $1
              |]
        let encoderXs = (ident^._Wrapped' >$ HE.param (HE.nonNullable HE.int8))      
        let decoderXs = HD.singleRow $ HD.column (HD.nonNullable HD.jsonb)
        json <- Hasql.Session.statement () (HS.Statement sqlXs encoderXs decoderXs False)
        let xse :: Result [ExGradeRange] = fromJSON json
        case xse of 
          Data.Aeson.Error e -> 
            throwError $ 
              Hasql.Session.QueryError 
              (e^.stext.textbs) 
              [] 
              (Hasql.Session.ClientError Nothing)
          Success xs -> do 
            let sql = 
                  [i|
                    with up as (
                      update "edgeNode"."UserQualification" 
                      set "qualificationSkillLevel" = $2
                      where id = $1
                      returning "qualificationKey" as k) 
                    select coalesce(array_agg("key"), array[]::int8[])
                    from "edgeNode"."Trajectory" as tr 
                    left join "edgeNode"."QualificationDependency" as qd
                    on tr."qualificationKey" = qd."key" 
                    where dependency = (select k from up) and tr."user" = $3
                  |]
            let encoder = 
                  contramap (^._1._Wrapped') (HE.param (HE.nonNullable HE.int8)) <>  
                  contramap (^._2.to toJSON) (HE.param (HE.nonNullable HE.jsonb)) <>
                  contramap (^._3._Wrapped') (HE.param (HE.nonNullable HE.int8)) 
            let decoder = 
                  HD.singleRow $ 
                  HD.column $ 
                  HD.nonNullable $ 
                  HD.array 
                  (HD.dimension 
                   replicateM 
                   (HD.element 
                    (HD.nonNullable 
                     HD.int8) <&> 
                     (^.from _Wrapped'))) 
            for (search skill xs) $ \new -> Hasql.Session.statement (ident, new, uid) (HS.Statement sql encoder decoder False)
    search :: Request_ValueSkill -> [ExGradeRange] -> Either T.Text UserQualificationSkill
    search skill xs =
      case skill of
        Request_ValueSkillQualificatonSkillLevel x ->
          if x `elem` 
             rights 
             [ exGradeRangeGrade 
             | ExGradeRange exGradeRangeGrade _ <- xs ]
          then Right $ UserQualificationSkillQualificatonSkillLevel x
          else Left [i|skill is out of range: #{show x}|]
        Request_ValueSkillLanguageSkillLevel x ->
          if x `elem` 
            lefts 
            [ exGradeRangeGrade 
            | ExGradeRange exGradeRangeGrade _ <- xs ]
          then Right $ UserQualificationSkillLanguageSkillLevel x 
          else Left [i|skill is out of range: #{show x}|]