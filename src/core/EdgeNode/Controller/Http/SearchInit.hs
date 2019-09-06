{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.SearchInit (controller) where

import EdgeNode.Provider.Qualification
import EdgeNode.Error

import Katip
import KatipController
import Json
import Database.Action
import qualified Data.Text as T
import Data.Either.Unwrap
import Control.Lens.Iso.Extended
import Control.Lens
import qualified Hasql.Session as Hasql.Session
import Data.Bifunctor

controller :: KatipController (Alternative (Error T.Text) [XQualificationFullInfo])
controller = 
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql (const action) raw
    whenLeft x ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)     
    return $ first mkErr x^.eitherToAlt

action :: Hasql.Session.Session [XQualificationFullInfo]    
action = undefined