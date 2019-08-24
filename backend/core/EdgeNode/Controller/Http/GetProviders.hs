{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Http.GetProviders (controller) where

import EdgeNode.Api.Http.User.GetProviders
import EdgeNode.Error
import EdgeNode.Lang
import EdgeNode.Model.Provider

import RetrofitProto
import KatipController
import ReliefJsonData
import Data.Generics.Product
import Control.Lens
import Database.Action
import Katip
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Exception
import Control.Lens.Iso.Extended
import qualified Data.Text as T
import Data.Bifunctor
import Control.Monad.Error.Class 
       ( throwError
       , catchError)
import Control.Exception (fromException)
import Data.Vector.Lens
import Data.Generics.Sum
import Data.Traversable
import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Data.String.Interpolate
import Control.Monad
import Data.Word

controller :: GetProvidersRequest -> KatipController (Alternative (Error T.Text) GetProvidersResponse)
controller req =
  do
    let ident = req^._Wrapped'.field @"requestIdent"
    let Just lang = req^?_Wrapped'.field @"requestLang".field @"enumerated"._Right
    let cursor = req^._Wrapped'.field @"requestCursor"
    orm <- fmap (^.katipEnv.ormDB) ask
    let logErr e = 
          do $(logTM) ErrorS (logStr (show e))
             throwError e    
    let mkError e =  
         case fromException e 
              :: Maybe Groundhog of
           Just (Action x) -> ResponseError (x^.stext)
           Just (Common x) -> ServerError $ InternalServerError (x^.stextl)
           _ -> ServerError $ InternalServerError "unkonwn server error, please pay a visit to log"
    (^.eitherToAlt) . first mkError <$> 
     runTryDbConnGH (action cursor lang ident `catchError` logErr) orm
    
action :: Word32 -> Language -> Maybe RequestIdent -> EdgeNodeAction GetProvidersResponse
action _ _ Nothing = throwError $ Action "ident blank"
action cursor lang (Just ident) = 
  do        
    exam <- for (ident^?_Ctor @"RequestIdentStateExamId") getExam
    degree <- for (ident^?_Ctor @"RequestIdentHigherDegreeId") getDegree
    diploma <- for (ident^?_Ctor @"RequestIdentInternationalDiplomaId") getDiploma
    ilang <- for (ident^?_Ctor @"RequestIdentLanguageStandardId") getILang
    maybe (throwError (Action "ident not found"))
          (return . GetProvidersResponse . Response . (^.vector))                
          (join (exam <|> degree <|> diploma <|> ilang))
  where 
    getExam ident =  
      do 
        let sql = 
             [i|select p.id, p."providerTitle", p."providerCountry" 
                from "edgeNode"."StateExamProvider" as e
                join "edgeNode"."Provider" as p
                on e.provider_id = p.id 
                where e.state_exam_id = ? and p."providerCountry" = ?
                order by p."providerTitle" asc 
                limit 10 offset ?
             |]
        stream <- queryRaw False sql 
         [ PersistInt64 (ident^.field @"stateExamIdValue")
         , PersistText (fromLanguage lang^.stext)
         , PersistInt64 (fromIntegral cursor)]
        xs <- liftIO $ streamToList stream
        $(logTM) DebugS (logStr ("exam.streamToList: " ++ show xs))
        xs' <- forM xs $ \(x:xs) -> do 
          ident <- fromSinglePersistValue x
          (v, _) <- fromPersistValues xs
          return $ XProvider ident v
        return $ if null xs' then Nothing else Just xs'
    getDegree ident = 
      do 
        let sql = 
             [i|select p.id, p."providerTitle", p."providerCountry" 
               from "edgeNode"."HigherDegreeProvider" as d
               join "edgeNode"."Provider" as p
               on d.provider_id = p.id 
               where d.higher_degree_id = ? and p."providerCountry" = ?
               order by p."providerTitle" asc 
               limit 10 offset ?
             |]
        stream <- queryRaw False sql 
         [ PersistInt64 (ident^.field @"higherDegreeIdValue")
         , PersistText (fromLanguage lang^.stext)
          , PersistInt64 (fromIntegral cursor)]
        xs <- liftIO $ streamToList stream
        $(logTM) DebugS (logStr ("degree.streamToList: " ++ show xs))
        xs' <- forM xs $ \(x:xs) -> do 
          ident <- fromSinglePersistValue x
          (v, _) <- fromPersistValues xs
          return $ XProvider ident v
        return $ if null xs' then Nothing else Just xs'     
    getDiploma _ =  throwError $ Action "international diploma not supported"
    getILang _ = throwError $ Action "language standard not supported"