{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.GetCategories (controller) where

import EdgeNode.Error
import EdgeNode.Model.Category
import EdgeNode.Api.Http.User.GetCategories

import TH.Proto
import KatipController
import Json
import Data.Aeson.Unit
import Control.Lens
import Database.Action
import Katip
import Control.Lens.Iso.Extended
import Data.Vector.Lens
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Hasql.Session as Hasql.Session
import Data.Functor
import Data.String.Interpolate
import Proto3.Suite.Types
import Data.Aeson (eitherDecode)
import Data.Bifunctor

controller :: KatipController (Alternative (Error Unit) GetCategoriesResponse)
controller =
  do
    hasql <- (^.katipEnv.hasqlDb) `fmap` ask
    x <- runTryDbConnHasql (const action) hasql
    let mkErr e = 
         $(logTM) ErrorS (logStr (show e)) $> 
         Error (ServerError (InternalServerError (show e^.stextl)))
    either mkErr (return . Fortune) x
 
action :: Hasql.Session.Session GetCategoriesResponse
action = 
  do
    exams <- Hasql.Session.statement () getStateExams
    degrees <- Hasql.Session.statement () getHigherDegrees
    diplomas <- Hasql.Session.statement () getInternationalDiplomas
    langs <- Hasql.Session.statement () getLanguageStandards
    let resp = 
         Response 
         (Just (Response_XStateExamValue 
                (show TH.Proto.StateExam^.stext.from lazytext) 
                (exams^.vector))) 
         (Just (Response_XHigherDegreeValue 
                (show TH.Proto.HigherDegree^.stext.from lazytext) 
                (degrees^.vector))) 
         (Just (Response_XInternationalDiplomaValue 
                (show TH.Proto.InternationalDiploma^.stext.from lazytext) 
                (diplomas^.vector))) 
         (Just (Response_XLanguageStandardValue 
                (show TH.Proto.LanguageStandard^.stext.from lazytext) 
                (langs^.vector)))
    return $ GetCategoriesResponse resp

getStateExams :: HS.Statement () [XStateExam]
getStateExams = HS.Statement sql HE.noParams decoder False
  where sql =
          [i|select s.id, s."stateExamTitle", p."providerCountry"  
          from "edgeNode"."StateExam" as s 
          join "edgeNode"."StateExamProvider" as sp 
          on s.id = sp.state_exam_id 
          join "edgeNode"."Provider" as p 
          on sp.provider_id = p.id|]     
        decoder = 
          HD.rowList $ do
            ident <- HD.column (HD.nonNullable HD.int8) <&> (^._Unwrapped')
            stateExamTitle <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
            ctry <- HD.column (HD.nonNullable (HD.enum (Just . (^.from stext.to toCountry))))
            let stateExamCountry = Enumerated (Right ctry)
            let value = EdgeNode.Model.Category.StateExam {..}
            return $ XStateExam (Just ident) (Just value)

getHigherDegrees :: HS.Statement () [XHigherDegree]
getHigherDegrees = HS.Statement sql HE.noParams decoder False
  where sql = 
          [i|select h.id, p."providerTitle", p."providerCountry"  
             from "edgeNode"."HigherDegree" as h 
             join "edgeNode"."HigherDegreeProvider" as hp 
             on h.id = hp.higher_degree_id 
             join "edgeNode"."Provider" as p 
             on hp.provider_id = p.id|]
        decoder = 
          HD.rowList $ do
            ident <- HD.column (HD.nonNullable HD.int8) <&> (^._Unwrapped')
            higherDegreeProvider <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
            ctry <- HD.column (HD.nonNullable (HD.enum (Just . (^.from stext.to toCountry))))
            let higherDegreeCountry = Enumerated (Right ctry)
            let value = EdgeNode.Model.Category.HigherDegree {..}
            return $ XHigherDegree (Just ident) (Just value)    

getInternationalDiplomas :: HS.Statement () [XInternationalDiploma]
getInternationalDiplomas = HS.Statement sql HE.noParams decoder False
  where sql = [i|select * from "edgeNode"."InternationalDiploma"|]
        decoder = 
          HD.rowList $ do
            ident <- HD.column (HD.nonNullable HD.int8) <&> (^._Unwrapped')
            internationalDiplomaTitle <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
            let value = EdgeNode.Model.Category.InternationalDiploma {..}
            return $ XInternationalDiploma (Just ident) (Just value)

getLanguageStandards :: HS.Statement () [XLanguageStandard]
getLanguageStandards = HS.Statement sql HE.noParams decoder False
  where sql = [i|select * from "edgeNode"."LanguageStandard"|]
        decoder = 
          HD.rowList $ do
            ident <- HD.column (HD.nonNullable HD.int8) <&> (^._Unwrapped')
            languageStandardStandard <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
            languageStandardGrade <- 
              HD.column $ HD.nonNullable
              (HD.jsonbBytes 
               ( first (^.stext) 
               . eitherDecode 
               . (^.from bytesLazy)))
            let value = EdgeNode.Model.Category.LanguageStandard {..}
            return $ XLanguageStandard (Just ident) (Just value)