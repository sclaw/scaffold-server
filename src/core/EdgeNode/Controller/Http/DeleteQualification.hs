{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Controller.Http.DeleteQualification (controller) where

import EdgeNode.Model.User
import EdgeNode.Model.Qualification
import EdgeNode.Model.User.Qualification
import qualified EdgeNode.Controller.Http.SaveTrajectory as SaveTrajectory 
import EdgeNode.Error

import Json
import Katip
import KatipController
import Database.Action
import Data.Aeson.Unit
import qualified Data.Text as T
import Data.Bifunctor
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.Either.Unwrap
import Control.Lens.Iso.Extended
import Control.Lens
import Data.String.Interpolate
import Data.Foldable
import Control.Monad

controller :: UserQualificationId -> UserId -> KatipController (Alternative (Error T.Text) Unit)
controller qid uid = 
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    let go = do
          e <- action qid uid
          traverse (\xs -> for_ xs $ SaveTrajectory.action uid . Just) e
    x <- runTryDbConnHasql (const go) raw
    whenLeft x ($(logTM) ErrorS . logStr . show)
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)
    return $ either (Json.Error . mkErr) ((^.eitherToAlt) . bimap ResponseError (const Unit)) x
     
action :: UserQualificationId -> UserId -> Hasql.Session.Session (Either T.Text [QualificationId])
action qid uid =
  do
    let sql = 
          [i|with del as 
              (delete from "edgeNode"."UserQualification" 
               where id = $1 and "userId" = $2
               returning "qualificationKey" as k)
               select coalesce(array_agg("key"), array[]::int8[])
               from "edgeNode"."Trajectory" as tr
               left join "edgeNode"."QualificationDependency" as qd
               on qd."key" = tr."qualificationKey" 
               where tr."user" = $2 and qd."dependency" = (select k from del)         
          |]
    let encoder =
         (qid^._Wrapped' >$ HE.param HE.int8) <>
         (uid^._Wrapped' >$ HE.param HE.int8)
    let decoder = HD.singleRow $ HD.column $ HD.array (HD.dimension replicateM (HD.element (HD.int8 <&> (^.from _Wrapped')))) 
    fmap Right $ Hasql.Session.statement () (HS.Statement sql encoder decoder False)