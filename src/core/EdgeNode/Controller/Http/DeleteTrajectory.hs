{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Controller.Http.DeleteTrajectory (controller) where

import EdgeNode.Model.User 
import EdgeNode.Error
import EdgeNode.Model.User.Trajectory

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

controller :: TrajectoryId -> UserId -> KatipController (Alternative (Error T.Text) Unit)
controller tid uid = 
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql (const (action tid uid)) raw
    whenLeft x ($(logTM) ErrorS . logStr . show)
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)
    return $ either (Json.Error . mkErr) ((^.eitherToAlt) . bimap ResponseError (const Unit)) x
     
action :: TrajectoryId -> UserId -> Hasql.Session.Session (Either T.Text ())
action tid uid =
  do
    let sql = [i|delete from "edgeNode"."Trajectory" where "user" = $2 and id = $1|]
    let encoder =
          (tid^._Wrapped' >$ HE.param HE.int8) <>
          (uid^._Wrapped' >$ HE.param HE.int8)
    fmap Right $ Hasql.Session.statement () (HS.Statement sql encoder HD.unit False)   
