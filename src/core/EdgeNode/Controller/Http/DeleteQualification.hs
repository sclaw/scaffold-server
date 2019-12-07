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
import EdgeNode.Statement.User (deleteQualification)

import Json
import KatipController
import Database.Transaction
import Data.Aeson.Unit
import qualified Data.Text as T
import Data.Bifunctor
import qualified Hasql.Session as Hasql.Session
import Control.Lens
import Data.Foldable
import Data.Traversable

controller :: UserQualificationId -> UserId -> KatipController (Alternative (Error T.Text) Unit)
controller qid uid = 
  do
    hasql <- (^.katipEnv.hasqlDbPool) `fmap` ask
    x <- katipTransaction hasql $ lift $ do 
      e <- action qid uid
      for e $ \xs -> for_ xs $ 
        SaveTrajectory.action uid . Just
    return $ (bimap ResponseError (const Unit) x)^.eitherToAlt
     
action :: UserQualificationId -> UserId -> Hasql.Session.Session (Either T.Text [QualificationId])
action qid uid = fmap Right $ Hasql.Session.statement (qid, uid) deleteQualification