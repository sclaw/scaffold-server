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
import EdgeNode.Statement.User (deleteQualification)

import Json
import Katip
import KatipController
import Database.Action
import Data.Aeson.Unit
import qualified Data.Text as T
import Data.Bifunctor
import qualified Hasql.Session as Hasql.Session
import Data.Either.Unwrap
import Control.Lens.Iso.Extended
import Control.Lens
import Data.Foldable

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
action qid uid = fmap Right $ Hasql.Session.statement (qid, uid) deleteQualification