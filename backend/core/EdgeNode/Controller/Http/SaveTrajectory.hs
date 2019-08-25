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
import Data.Maybe
import Data.Generics.Product


type RequestError = [WithField "qualificationId" (Maybe QualificationId) SaveTrajectoryError]

controller 
  :: UserId 
  -> SaveTrajectoryRequest 
  -> KatipController 
     (Alternative (Error RequestError) 
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
          (^?_Request.to ResponseError)) 
         (fromException e :: Maybe (Groundhog RequestError))
    (^.eitherToAlt) . first mkError <$> 
     runTryDbConnGH (action uid ident `catchError` logErr) orm

action :: UserId -> Maybe QualificationId -> EdgeNodeAction RequestError SaveTrajectoryResponse     
action _ = maybe (throwError (Request [val])) ok
  where 
    val :: 
      WithField 
      "qualificationId" 
      (Maybe QualificationId) 
      SaveTrajectoryError
    val = WithField Nothing TrajectoryQualificationNotPassed
    -- UserQualification, QualificationDependency
    ok _ = throwError (Request [val])