{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.GetProvider (controller) where

import EdgeNode.Api.Http.User.GetProvider
import EdgeNode.Error
import EdgeNode.Lang
import EdgeNode.Model.Provider

import Database.FullText.Search
import RetrofitProto
import KatipController
import ReliefJsonData
import Data.Generics.Product
import Control.Lens
import Database.Action
import Katip
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
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
import Data.Maybe
import Data.String.Interpolate
import Control.Monad

controller :: GetProviderRequest -> KatipController (Alternative (Error T.Text) GetProviderResponse)
controller req =
  do
    let ident = req^._Wrapped'.field @"requestIdent"
    let Just lang = req^?_Wrapped'.field @"requestLang".field @"enumerated"._Right
    let query = req^._Wrapped'.field @"requestQuery".lazytext
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
     runTryDbConnGH (action query lang ident `catchError` logErr) orm
    
action :: T.Text -> Language -> Maybe RequestIdent -> TryAction Groundhog KatipController Postgresql GetProviderResponse
action _ _ Nothing = throwError $ Action "ident blank"
action term lang (Just ident) = 
  do 
    xs :: [(ProviderId, Provider)] <- provider lang term
    let xs' = map (\(i, v) -> XProvider (Just i) (Just v)) $ 
              xs^..folded.filtered 
              ( (== fromLanguage lang) 
              . (^._2.field @"providerCountry".lazytext.from stext))
    $(logTM) DebugS (logStr ("full text: " ++ show xs'))          
    exam <- for (ident^?_Ctor @"RequestIdentStateExamId") getExam
    degree <- for (ident^?_Ctor @"RequestIdentHigherDegreeId") getDegree
    diploma <- for (ident^?_Ctor @"RequestIdentInternationalDiplomaId") getDiploma
    ilang <- for (ident^?_Ctor @"RequestIdentLanguageStandardId") getILang
    let pickOut (is :: [ProviderId]) = 
         [ x | x@(XProvider (Just ident) _) <- xs'
         , ident `elem` is
         ]
    xs'' <- maybe (throwError (Action "ident not found")) 
                  (return . pickOut) 
                  (join (exam <|> degree <|> diploma <|> ilang))
    return $ GetProviderResponse $ Response $ xs''^.vector
  where 
    getExam ident =  
      do 
        let sql = [i|select provider_id from "edgeNode"."StateExamProvider" where state_exam_id = ?|]
        stream <- queryRaw False sql [PersistInt64 (ident^.field @"stateExamIdValue")]
        xs <- liftIO $ streamToList stream
        $(logTM) DebugS (logStr ("exam.streamToList: " ++ show xs))
        xs' <- catMaybes `fmap` mapM (\x -> for (x^?_head) fromSinglePersistValue) xs
        return $ if null xs' then Nothing else Just xs'
    getDegree ident = 
      do 
        let sql = [i|select provider_id from "edgeNode"."HigherDegreeProvider" where higher_degree_id = ?|]
        stream <- queryRaw False sql [PersistInt64 (ident^.field @"higherDegreeIdValue")]
        xs <- liftIO $ streamToList stream
        $(logTM) DebugS (logStr ("degree.streamToList: " ++ show xs))
        xs' <- catMaybes `fmap` mapM (\x -> for (x^?_head) fromSinglePersistValue) xs
        return $ if null xs' then Nothing else Just xs'     
    getDiploma _ =  throwError $ Action "international diploma not supported"
    getILang _ = throwError $ Action "language standard not supported"