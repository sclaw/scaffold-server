{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module EdgeNode.Controller.Http.GetEducationLevelList (controller) where

import EdgeNode.Error

import RetrofitReqRespProto
import KatipController
import ReliefJsonData
import Data.Aeson.Unit
import Control.Lens
import Database.Action
import Katip
import qualified Hasql.Session as Hasql.Session
import Control.Lens.Iso.Extended
import Data.Functor

controller :: KatipController (Alternative (Error Unit) GetEducationLevelListResponse)
controller =
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql action raw
    let mkErr e = 
         $(logTM) ErrorS (logStr (show e)) $> 
         Error (ServerError 
                (InternalServerError 
                 (show e^.stextl)))
    either mkErr (return . Fortune) x

action :: KatipLoggerIO -> Hasql.Session.Session GetEducationLevelListResponse
action _ = undefined