{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.GetQualificationFullInfo (controller) where

import EdgeNode.Error
import EdgeNode.Api.Http.User.GetQualificationFullInfo
import EdgeNode.User.Qualification
import EdgeNode.Model.User

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
import Data.String.Interpolate
import Data.Typeable

controller :: UserId -> KatipController (Alternative (Error T.Text) GetQualificationFullInfoResponse)
controller userId = 
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql (action userId) raw
    whenLeft x ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)
    let mkResp = GetQualificationFullInfoResponse . Response . (^.vector)     
    return $ bimap mkErr mkResp x^.eitherToAlt

action :: UserId -> KatipLoggerIO -> Hasql.Session.Session [XUserQualificationFullinfo]
action userId _ =
  do
    let sql = 
         [i|select from "edgeNode"."#{show (typeOf (undefined :: UserQualification))}"|]
    let encoder = userId^._Wrapped' >$ HE.param HE.int8
    let decoder = undefined 
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)
