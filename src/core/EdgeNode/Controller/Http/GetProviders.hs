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
import EdgeNode.Model.Provider

import TH.Proto
import KatipController
import Json
import Control.Lens
import Database.Transaction
import Katip
import Control.Lens.Iso.Extended
import qualified Data.Text as T
import Control.Monad.Error.Class (throwError)
import Data.Vector.Lens
import Control.Monad.IO.Class
import Data.String.Interpolate
import Data.Int
import Data.Word
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Proto3.Suite.Types
import Control.Monad

controller :: WrapperCountry -> WrapperType -> Int64 -> Maybe Word32 -> KatipController (Alternative (Error T.Text) GetProvidersResponse)
controller country ty ident cursor =
  do
    hasql <- (^.katipEnv.hasqlDbPool) `fmap` ask
    x <- katipTransaction hasql $ (ask >>= lift . action country ty ident cursor)
    return $ Fortune x

action :: WrapperCountry -> WrapperType -> Int64 -> Maybe Word32 -> KatipLoggerIO -> Hasql.Session.Session GetProvidersResponse
action _ ty _ _ _
  | ty == InternationalDiploma || 
    ty == LanguageStandard = 
    throwError $ 
      Hasql.Session.QueryError 
      [i|next categories doesn't support providers 
         #{fromWrapperType InternationalDiploma}, 
         #{fromWrapperType LanguageStandard}|]
      []
      (Hasql.Session.ClientError Nothing)    
action country ty ident cursor logger = 
  fmap (GetProvidersResponse . Response . (^.vector)) 
  (case ty of StateExam -> getExam; HigherDegree -> getDegree)
  where
    getExam =
      do 
        let sql = 
              [i|select p.id, p."providerTitle", p."providerCountry" 
                 from "edgeNode"."StateExamProvider" as e
                 join "edgeNode"."Provider" as p
                 on e.provider_id = p.id 
                 where e.state_exam_id = $1 and p."providerCountry" = $2
                 order by p."providerTitle" asc 
                 limit 10 offset coalesce($3, 0)|]
        let encoder =
              (ident >$ HE.param (HE.nonNullable HE.int8)) <>
              (country^.isoWrapperCountry.stext >$ HE.param (HE.nonNullable HE.text)) <>
              (cursor^?_Just.integral >$ HE.param (HE.nullable HE.int4))
        let examDecoder = do
              ident <- HD.column (HD.nonNullable HD.int8) <&> (^._Unwrapped')
              title <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
              ctry <- HD.column (HD.nonNullable (HD.enum (Just . (^.from stext.to toCountry)))) 
              let value = Provider title (Enumerated (Right ctry))
              return $ XProvider (Just ident) (Just value) []
        let decoder = HD.rowList examDecoder
        let params = (ident, country^.isoWrapperCountry, cursor)      
        liftIO $ logger DebugS (logStr (sql^.from textbs.from stext ++ show params))
        Hasql.Session.statement () (HS.Statement sql encoder decoder False)          
    getDegree =
      do
        let sql = 
              [i|select p.id, p."providerTitle", p."providerCountry",
                 array(select distinct "qualificationProviderDegreeType" 
                       "qualificationProviderDegreeType"
                       from "edgeNode"."QualificationProvider" 
                       where "qualificationProviderKey" = p.id) 
                 from "edgeNode"."HigherDegreeProvider" as d
                 join "edgeNode"."Provider" as p
                 on d.provider_id = p.id 
                 where d.higher_degree_id = $1 and p."providerCountry" = $2
                 order by p."providerTitle" asc 
                 limit 10 offset coalesce($3, 0)|]
        let encoder =
              (ident >$ HE.param (HE.nonNullable HE.int8)) <>
              (country^.isoWrapperCountry.stext >$ HE.param (HE.nonNullable HE.text)) <>
              (cursor^?_Just.integral >$ HE.param (HE.nullable HE.int4))        
        let params = (ident, country^.isoWrapperCountry, cursor)
        let degreeDecoder = do 
             ident <- HD.column (HD.nonNullable HD.int8) <&> (^._Unwrapped')  
             title <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
             ctry <- HD.column (HD.nonNullable (HD.enum (Just . (^.from stext.to toCountry))))
             degrees <- HD.column (HD.nonNullable (HD.array (HD.dimension replicateM (HD.element (HD.nonNullable HD.text)))))
             let value = Provider title (Enumerated (Right ctry))
             return $ XProvider (Just ident) (Just value) ((degrees^..traversed.from lazytext)^.vector)          
        let decoder = HD.rowList degreeDecoder      
        liftIO $ logger DebugS (logStr (sql^.from textbs.from stext ++ show params))
        Hasql.Session.statement () (HS.Statement sql encoder decoder False)