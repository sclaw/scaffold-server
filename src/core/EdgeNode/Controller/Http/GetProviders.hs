{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module EdgeNode.Controller.Http.GetProviders (controller) where

import EdgeNode.Api.Http.User.GetProviders
import EdgeNode.Error
import EdgeNode.Model.Provider

import Proto
import KatipController
import Json
import Control.Lens
import Database.Action
import Katip
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Exception
import Control.Lens.Iso.Extended
import qualified Data.Text as T
import Data.Bifunctor
import Control.Monad.Error.Class (throwError, catchError)
import Control.Exception (fromException)
import Data.Vector.Lens
import Data.Traversable
import Control.Monad.IO.Class
import Data.String.Interpolate
import Data.Int
import Data.Word
import Data.Maybe
import Database.Render

controller :: WrapperCountry -> WrapperType -> Int64 -> Maybe Word32 -> KatipController (Alternative (Error T.Text) GetProvidersResponse)
controller country ty ident cursor =
  do
    orm <- fmap (^.katipEnv.ormDB) ask
    let logErr e = 
          do $(logTM) ErrorS (logStr (show e))
             throwError e    
    let mkError e =  
         case fromException e 
              :: Maybe (Groundhog ()) of
           Just (Action x) -> ResponseError (x^.stext)
           Just (Common x) -> ServerError $ InternalServerError (x^.stextl)
           _ -> ServerError $ InternalServerError "unkonwn server error, please pay a visit to log"
    (^.eitherToAlt) . first mkError <$> 
     runTryDbConnGH (action country ty ident cursor `catchError` logErr) orm
    
action :: WrapperCountry -> WrapperType -> Int64 -> Maybe Word32 -> EdgeNodeAction () GetProvidersResponse
action _ ty _ _ 
  | ty == InternationalDiploma || 
    ty == LanguageStandard = 
    throwError $ Action 
      [i|next categories doesn't support providers 
         #{fromWrapperType InternationalDiploma}, 
         #{fromWrapperType LanguageStandard}|]  
action country ty ident cursor = 
  do        
    response <- fmap (fmap (GetProvidersResponse . Response . (^.vector))) 
                (case ty of StateExam -> getExam; HigherDegree -> getDegree)
    return $ fromMaybe (GetProvidersResponse (Response [])) response
  where 
    getExam =  
      do 
        let sql = 
             [i|select p.id, p."providerTitle", p."providerCountry" 
                from "edgeNode"."StateExamProvider" as e
                join "edgeNode"."Provider" as p
                on e.provider_id = p.id 
                where e.state_exam_id = ? and p."providerCountry" = ?
                order by p."providerTitle" asc 
                limit 10 offset coalesce(?, 0)
             |]
        let vs = 
             [ PersistInt64 ident
             , PersistText (country^.isoWrapperCountry.stext)
             , toPrimitivePersistValue (fmap fromIntegral cursor :: Maybe Int32)]     
        stream <- queryRaw False sql vs
        xs <- liftIO $ streamToList stream
        $(logTM) DebugS (logStr ("query: " ++ rawSql sql vs))
        $(logTM) DebugS (logStr ("exam.streamToList: " ++ show xs))
        xs' <- forM xs $ \(x:xs) -> do 
          ident <- fromSinglePersistValue x
          (v, _) <- fromPersistValues xs
          return $ XProvider ident v
        return $ if null xs' then Nothing else Just xs'
    getDegree = 
      do 
        let sql = 
             [i|select p.id, p."providerTitle", p."providerCountry" 
               from "edgeNode"."HigherDegreeProvider" as d
               join "edgeNode"."Provider" as p
               on d.provider_id = p.id 
               where d.higher_degree_id = ? and p."providerCountry" = ?
               order by p."providerTitle" asc 
               limit 10 offset coalesce(?, 0)
             |]
        stream <- queryRaw False sql 
         [ PersistInt64 ident
         , PersistText (country^.isoWrapperCountry.stext)
         , toPrimitivePersistValue (fmap fromIntegral cursor :: Maybe Int32)]
        xs <- liftIO $ streamToList stream
        $(logTM) DebugS (logStr ("degree.streamToList: " ++ show xs))
        xs' <- forM xs $ \(x:xs) -> do 
          ident <- fromSinglePersistValue x
          (v, _) <- fromPersistValues xs
          return $ XProvider ident v
        return $ if null xs' then Nothing else Just xs'