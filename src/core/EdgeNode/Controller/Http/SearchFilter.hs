{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Http.SearchFilter (controller) where

import EdgeNode.Search.Filter
import EdgeNode.Error
import EdgeNode.Iso

import Katip
import KatipController
import Json
import Database.Action
import qualified Data.Text as T
import Data.Either.Unwrap
import Control.Lens.Iso.Extended
import Control.Lens
import qualified Hasql.Session as Hasql.Session
import Data.Bifunctor
import Data.String.Interpolate
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Control.Monad
import Data.Vector.Lens

controller :: KatipController (Alternative (Error T.Text) Filter)
controller = 
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql (const action) raw
    whenLeft x ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)     
    return $ first mkErr x^.eitherToAlt

action :: Hasql.Session.Session Filter
action = do
  let sql = 
        [i|select 
            (select array(select jsonb_array_elements_text("academicAreas")) 
             from "edgeNode"."QualificationProviderFeatures"),
            (select array_agg(distinct language) 
             from "edgeNode"."QualificationProviderFeatures"),
            (select array_agg(distinct "providerCountry")  
             from "edgeNode"."Provider"),
            (select array_agg(distinct "qualificationProviderDegreeType") 
             from "edgeNode"."QualificationProvider")
        |]
  let decoder = HD.singleRow $ do 
        areas <- HD.column (HD.array (HD.dimension replicateM (HD.element HD.text)))
        let filterAcademicAreas = Just $ AcademicAreas ((areas^..traverse.from lazytext)^.vector)
        langs <- HD.column (HD.array (HD.dimension replicateM (HD.element (HD.text <&> (^.from language)))))   
        let filterLanguages = Just $ Languages (langs^.vector) 
        countries <- HD.column (HD.array (HD.dimension replicateM (HD.element (HD.text <&> (^.from country)))))
        let filterCountries = Just $ Countries (countries^.vector)
        degrees <- HD.column (HD.array (HD.dimension replicateM (HD.element HD.text)))
        let filterDegrees = Just $ Degrees ((areas^..traverse.from lazytext)^.vector)
        return Filter {..}
  Hasql.Session.statement () (HS.Statement sql HE.unit decoder False)