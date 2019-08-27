{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.SaveTrajectory (controller) where

import EdgeNode.Model.User (UserId)
import EdgeNode.Provider.Qualification
import EdgeNode.Error
import EdgeNode.Model.Qualification
import EdgeNode.Category ()
import EdgeNode.Api.Http.User.SaveTrajectory (Response (..))
import EdgeNode.Model.User.Trajectory

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
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Data.String.Interpolate
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Sequence as Seq
import Data.Traversable
import Data.Aeson
import Database.Groundhog.Instances ()
import Orm.PersistField ()

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
action uid = maybe (throwError (Request [val])) ok
  where 
    val :: 
      WithField 
      "qualificationId" 
      (Maybe QualificationId) 
      SaveTrajectoryError
    val = WithField Nothing TrajectoryQualificationNotPassed
    ok ident = 
      do
        let sqlGet = 
             [i|select qp."qualificationProviderCategoryType", 
                  qd."minRequiredGrade", 
                  uq."qualificationSkillLevel",
                  qp."qualificationProviderGradeRange"   
                from "edgeNode"."QualificationDependency" as qd
                join "edgeNode"."QualificationProvider" as qp
                  on qd.dependency = qp.id
                left outer join "edgeNode"."UserQualification" as uq
                  on qd.dependency = uq."qualificationKey"
                where qd.key = ?|]
        streamGet <- queryRaw False sqlGet 
         [toPrimitivePersistValue ident]
        xs <- liftIO $ streamToList streamGet
        $(logTM) DebugS (logStr ([i|raw data -> #{show xs}|] :: String))
        when (null xs) $
          throwError $ Request 
            [WithField 
             (Just ident) 
             QualificationDependencyNotFound]
        let mkQDiff cnt score = QualificationDiff (score / fromIntegral cnt)     
        diff :: QualificationDiff <- uncurry mkQDiff <$> foldM accScore (1, 0) xs
        let sqlPut = 
              [i|insert into "edgeNode"."Trajectory" 
                 ("user", "qualificationKey", "overlap") 
                 values (?, ?, ?) returning id|]  
        streamPut <- queryRaw False sqlPut
          [toPrimitivePersistValue uid,
           toPrimitivePersistValue ident,
           toPrimitivePersistValue 
           (ValueWrapper (toJSON diff))]
        row <- firstRow streamPut
        trajectoryIdent <- for row (fromSinglePersistValue . head)
        return $ SaveTrajectoryResponse $ Response trajectoryIdent 

accScore :: (Int, Double) -> [PersistValue] -> EdgeNodeAction RequestError (Int, Double)  
accScore score xs = 
  do
    let seq = xs^.seql
    ty <- maybe (error "persist field category type not found") (fmap toType . fromSinglePersistValue) (seq Seq.!? 1)
    qual :: ValueWrapper <- maybe (error "persist field required grade not found") fromSinglePersistValue (seq Seq.!? 2)
    user :: Maybe ValueWrapper <- traverse fromSinglePersistValue $ seq Seq.!? 3
    range :: Maybe ExGradeRange <- traverse fromSinglePersistValue $ seq Seq.!? 4
    return score