{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module EdgeNode.Controller.Http.SaveTrajectory (controller, action) where

import EdgeNode.Model.User (UserId)
import EdgeNode.Provider.Qualification
import EdgeNode.Error 
import EdgeNode.Model.Qualification ()
import EdgeNode.Model.User.Trajectory
import EdgeNode.Api.Http.User.SaveTrajectory (Response (..))

import Proto
import KatipController
import Json
import Control.Lens
import Database.Action
import Katip
import Control.Lens.Iso.Extended
import Data.Generics.Product
import qualified Data.Text as T
import qualified Hasql.Session as Hasql.Session
import Data.Either.Unwrap
import Control.Monad
import Data.Traversable
import Data.Either.Combinators (maybeToRight)
import Data.String.Interpolate
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.Aeson

controller 
  :: SaveTrajectoryRequest
  -> UserId 
  -> KatipController 
     (Alternative (Error T.Text) 
      SaveTrajectoryResponse)    
controller req uid  =
  do
    let ident = req^._Wrapped'.field @"requestIdent"
    raw <- (^.katipEnv.hasqlDb) `fmap` ask
    x <- runTryDbConnHasql (const (action uid ident)) raw
    whenLeft x ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)     
    case x of 
      Left e -> return $ Json.Error $ mkErr e
      Right (Left e) -> return $ Json.Error $ ResponseError e
      Right (Right resp) -> return $ Fortune resp

action :: UserId -> Maybe QualificationId -> Hasql.Session.Session (Either T.Text SaveTrajectoryResponse)
action uid qidm = fmap (join . maybeToRight "qualification request id empty") $ for qidm go
  where 
    go qid = 
      do
        let sql = 
             [i|
              select sum(("minRequiredGrade"->0)::int8), 
                     coalesce(sum(("qualificationSkillLevel"#>'{qualificatonSkillLevel, value, integral}')::int8), 0)
              from (select 
                      qd.key, 
                      qd.dependency, 
                      qd."minRequiredGrade", 
                      qp."qualificationProviderTitle" 
                    from "edgeNode"."QualificationDependency" as qd
                    left join "edgeNode"."QualificationProvider" as qp
                    on qd.dependency = qp.id) as qd
              left join "edgeNode"."UserQualification" as uq
              on qd.dependency = uq."qualificationKey" and "userId" = $2 
              where qd."key" = $1      
             |] 
        let encoder = 
             (qid^._Wrapped' >$ HE.param (HE.nonNullable HE.int8)) <>
             (uid^._Wrapped' >$ HE.param (HE.nonNullable HE.int8))
        let decoder = HD.singleRow $ do
              min <- HD.column (HD.nonNullable HD.int8)
              skill <- HD.column (HD.nonNullable HD.int8)
              return (min, skill)
        (min, skill) <- Hasql.Session.statement () (HS.Statement sql encoder decoder False)
        let diff = QualificationDiff ((fromIntegral skill * 100) / fromIntegral min)
        let sqlT = 
              [i|
                insert into "edgeNode"."Trajectory" 
                ("user", "qualificationKey", "overlap") 
                values ($2, $1, $3) 
                on conflict ("qualificationKey", "user")
                do update
                set "overlap" = excluded."overlap"  
                returning id
              |]
        let encoder =
             (qid^._Wrapped' >$ HE.param (HE.nonNullable HE.int8)) <>
             (uid^._Wrapped' >$ HE.param (HE.nonNullable HE.int8)) <>
             (toJSON diff >$ HE.param (HE.nonNullable HE.jsonb))
        let decoder = HD.singleRow $ HD.column (HD.nonNullable HD.int8)
        ident <- Hasql.Session.statement () (HS.Statement sqlT encoder decoder False)
        return $ Right $ SaveTrajectoryResponse $ Response (Just (TrajectoryId ident))