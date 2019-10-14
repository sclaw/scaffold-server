{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Http.SearchApplyFilter (controller) where

import EdgeNode.Controller.Http.SearchInit (action)
import EdgeNode.Provider.Qualification
import EdgeNode.Error
import EdgeNode.Model.Qualification ()
import EdgeNode.Model.User
import EdgeNode.Search.Filter

import Katip
import KatipController
import Json
import Database.Action
import qualified Data.Text as T
import Data.Either.Unwrap
import Control.Lens.Iso.Extended
import Control.Lens
import Data.Bifunctor

controller :: Filter -> Maybe UserId -> KatipController (Alternative (Error T.Text) [XQualificationFullInfo])
controller filter ident = 
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql (const (action ident (Just filter))) raw
    whenLeft x ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)     
    return $ first mkErr x^.eitherToAlt