{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Http.SearchApplyFilter (controller) where

import EdgeNode.Controller.Http.SearchInit (action)
import EdgeNode.Provider.Qualification
import EdgeNode.Model.Qualification ()
import EdgeNode.Model.User
import EdgeNode.Search.Filter

import KatipController
import Json
import Database.Transaction
import qualified Data.Text as T
import Control.Lens

controller :: Filter -> Maybe UserId -> KatipController (Alternative (Error T.Text) [XQualificationFullInfo])
controller filter ident = 
  do
    hasql <- (^.katipEnv.hasqlDbPool) `fmap` ask
    fmap Fortune $ katipTransaction hasql $ lift $ action ident (Just filter)