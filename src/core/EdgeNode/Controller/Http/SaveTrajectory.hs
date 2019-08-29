{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Http.SaveTrajectory (controller) where

import EdgeNode.Model.User (UserId)
import EdgeNode.Provider.Qualification
import EdgeNode.Error
import EdgeNode.Model.Qualification
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
import qualified Data.Aeson as Aeson
import Database.Groundhog.Instances ()
import Orm.PersistField ()
import Data.Monoid

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
             [i|select distinct on 
                 ( qp."qualificationProviderCategoryType"
                 , qd."minRequiredGrade")
                 qp."qualificationProviderCategoryType", 
                 qd."minRequiredGrade",                        
                 uq."qualificationSkillLevel",
                 qp."qualificationProviderGradeRange"   
                from "edgeNode"."QualificationDependency" as qd
                join "edgeNode"."QualificationProvider" as qp
                  on qd.dependency = qp.id
                cross join "edgeNode"."UserQualification" as uq
                where qd.key = ? and uq."userId" = ?;|]
        streamGet <- queryRaw False sqlGet 
         [toPrimitivePersistValue ident, 
          toPrimitivePersistValue uid]
        xs <- liftIO $ streamToList streamGet
        $(logTM) DebugS (logStr ([i|raw data -> #{show xs}|] :: String))
        when (null xs) $
          throwError $ Request 
            [WithField 
             (Just ident) 
             QualificationDependencyNotFound]
        let mkQDiff 0 _ = error [i|counter is zero|]
            mkQDiff cnt score = QualificationDiff (getSum score / fromIntegral (getSum cnt))     
        diff :: QualificationDiff <- uncurry mkQDiff <$> foldM accScore (Sum 0, Sum 0) xs

        $(logTM) DebugS (logStr ([i|qualififcation diff: #{show diff}|] :: String)) 

        let sqlPut = 
              [i|insert into "edgeNode"."Trajectory" 
                 ("user", "qualificationKey", "overlap") 
                 values (?, ?, ?) returning id|]  
        streamPut <- queryRaw False sqlPut
          [toPrimitivePersistValue uid,
           toPrimitivePersistValue ident,
           toPrimitivePersistValue 
           (ValueWrapper (Aeson.toJSON diff))]
        row <- firstRow streamPut
        trajectoryIdent <- for row (fromSinglePersistValue . head)
        return $ SaveTrajectoryResponse $ Response trajectoryIdent 

accScore :: (Sum Int, Sum Double) -> [PersistValue] -> EdgeNodeAction RequestError (Sum Int, Sum Double)  
accScore score xs = 
  do
    let seq = xs^.seql
    ty <- maybe (error "persist field category type not found") 
          (fmap toWrapperType . fromSinglePersistValue) (seq Seq.!? 0)
    qual :: ValueWrapper <- 
      maybe (error "persist field required grade not found") 
      fromSinglePersistValue (seq Seq.!? 1)
    user :: Maybe ValueWrapper <- 
      maybe (error "persist field user grade not found")
      fromSinglePersistValue (seq Seq.!? 2)
    range <- 
      maybe (error "persist field grade range not found") 
      (fmap Aeson.fromJSON . fromSinglePersistValue) (seq Seq.!? 3)
    case range of 
      Aeson.Error e -> throwError $ Action [i|range decode error: #{show range}|]
      Aeson.Success xs -> return $ mkScore score ty (qual^.coerced) (user^?_Just.coerced) xs
    
mkScore :: (Sum Int, Sum Double) -> WrapperType -> Aeson.Value -> Maybe Aeson.Value -> [ExGradeRange] -> (Sum Int, Sum Double)
mkScore score ty qualVal userVal xs = 
  case result of 
    Aeson.Success x -> x 
    Aeson.Error e -> error [i|from json error: #{show ty}, #{show qualVal}, #{show userVal}|] 
  where 
    go lens = do 
      qual <- Aeson.fromJSON qualVal 
      userm <- traverse Aeson.fromJSON userVal
      let xs' =
           [( exGradeRangeRank
            , exGradeRangeGrade^?lens) 
            | ExGradeRange {..} <- xs]
      return $ maybe (first (+ 1) score) (cmp xs' qual) userm
    result = 
      case ty of 
        StateExam -> go _Right
        HigherDegree -> go _Right
        InternationalDiploma -> 
          error [i|unsupported type: #{fromWrapperType InternationalDiploma}|]
        LanguageStandard -> go _Left
    cmp xs qual user 
      | searchIdx user xs >= searchIdx qual xs = bimap (+ 1) (+ 1) score
      | otherwise = bimap (+ 1) (fmap (+ 0.5)) score
    searchIdx g [] = error [i|grade not found: #{show g}|]
    searchIdx g ((i, g'):gs) = if g == g' then i else searchIdx g gs 