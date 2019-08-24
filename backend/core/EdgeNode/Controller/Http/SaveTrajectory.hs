{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Http.SaveTrajectory (controller) where

import EdgeNode.Model.User (UserId)
import EdgeNode.Provider.Qualification
import EdgeNode.Error

import RetrofitProto
import KatipController
import ReliefJsonData
import Data.Aeson.WithField
import Control.Lens
import Database.Action
import Katip
import Control.Lens.Iso.Extended
import Database.Exception
import Data.Bifunctor
import Control.Monad.Error.Class 
       ( throwError
       , catchError)
import Control.Exception (fromException)
import Data.Aeson.Lens
import Data.Aeson
import Data.Maybe
import Data.Generics.Product

controller 
  :: UserId 
  -> SaveTrajectoryRequest 
  -> KatipController 
     (Alternative (Error [WithField "qualificationId" (Maybe QualificationId) SaveTrajectoryError]) 
      SaveTrajectoryResponse)    
controller uid req =
  do
    let ident = req^._Wrapped'.field @"requestIdent"
    orm <- fmap (^.katipEnv.ormDB) ask
    let logErr e = 
          do $(logTM) ErrorS (logStr (show e))
             throwError e   
    let mkError e = 
         maybe 
         (ServerError (InternalServerError (show e^.stextl))) 
         (fromMaybe (error "panic!!!!") . 
          (^?_Request._JSON.to ResponseError)) 
         (fromException e :: Maybe Groundhog)
    (^.eitherToAlt) . first mkError <$> 
     runTryDbConnGH (action uid ident `catchError` logErr) orm

action :: UserId -> Maybe QualificationId -> EdgeNodeAction SaveTrajectoryResponse     
action _ = maybe (throwError (Request (toJSON val))) ok
  where 
    val :: WithField 
           "qualificationId" 
           (Maybe QualificationId) 
           SaveTrajectoryError
    val = WithField Nothing TrajectoryQualificationNotPassed
    -- UserQualification, QualificationDependency
    ok _ = throwError (Request (toJSON [val]))