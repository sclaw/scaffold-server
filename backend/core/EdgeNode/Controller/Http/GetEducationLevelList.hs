{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Http.GetEducationLevelList (controller) where

import EdgeNode.Error
import EdgeNode.Model.EducationLevel
import EdgeNode.Api.Http.User.GetEducationLevelList

import RetrofitProto
import KatipController
import ReliefJsonData
import Data.Aeson.Unit
import Control.Lens
import Database.Action
import Katip
import Control.Lens.Iso.Extended
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Database.Exception
import Data.Bifunctor
import Control.Monad.Error.Class 
       ( throwError
       , catchError)
import Control.Exception (fromException)
import Data.Vector.Lens
import Data.Default.Class.Extended
import Data.Generics.Product

controller :: KatipController (Alternative (Error Unit) GetEducationLevelListResponse)
controller =
  do
    orm <- fmap (^.katipEnv.ormDB) ask
    let logErr e = 
          do $(logTM) ErrorS (logStr (show e))
             throwError e    
    let mkError e = 
         maybe 
         (ServerError (InternalServerError (show e^.stextl))) 
         (const (ResponseError Unit)) 
         (fromException e :: Maybe Groundhog)
    (^.eitherToAlt) . first mkError <$> 
     runTryDbConnGH (action `catchError` logErr) orm

action :: TryAction Groundhog KatipController Postgresql GetEducationLevelListResponse
action = 
  do
    exams :: [(AutoKey StateExam, StateExam)] <- selectAll
    degrees :: [(AutoKey HigherDegree, HigherDegree)] <- selectAll
    diplomas :: [(AutoKey InternationalDiploma, InternationalDiploma)] <- selectAll 
    langs :: [(AutoKey LanguageStandard, LanguageStandard)] <- selectAll
    let exams' = flip map exams $ \(k, v) ->
         (def :: XStateExam) 
         & field @"xstateExamIdent" ?~ (k^.from autokey) 
         & field @"xstateExamValue" ?~ v
    let degrees' = flip map degrees $ \(k, v) ->
         (def :: XHigherDegree) 
         & field @"xhigherDegreeIdent" ?~ (k^.from autokey) 
         & field @"xhigherDegreeValue" ?~ v
    let diplomas' = flip map diplomas $ \(k, v) ->
         (def :: XInternationalDiploma) 
         & field @"xinternationalDiplomaIdent" ?~ (k^.from autokey) 
         & field @"xinternationalDiplomaValue" ?~ v
    let langs' = flip map langs $ \(k, v) ->
         (def :: XLanguageStandard) 
         & field @"xlanguageStandardIdent" ?~ (k^.from autokey) 
         & field @"xlanguageStandardValue" ?~ v
    let resp = Response (exams'^.vector) (degrees'^.vector) (diplomas'^.vector) (langs'^.vector)
    return $ GetEducationLevelListResponse resp