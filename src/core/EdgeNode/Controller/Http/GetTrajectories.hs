{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Controller.Http.GetTrajectories (controller) where

import EdgeNode.Model.User (UserId)
import EdgeNode.Error
import EdgeNode.Api.Http.User.GetTrajectories
import EdgeNode.Model.User.Trajectory
import Time.Time

import Proto
import Katip
import KatipController
import Json
import qualified Data.Text as T
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Database.Action (runTryDbConnHasql)
import Control.Lens.Iso.Extended
import Data.Functor
import Control.Lens
import Control.Monad.IO.Class
import Data.Vector.Lens
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import Proto3.Suite.Types
import Data.Time.Clock.POSIX
import Data.String.Interpolate

controller :: UserId -> KatipController (Alternative (Error T.Text) GetTrajectoriesResponse)    
controller uid =
  do 
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql (action uid) raw
    let mkErr e = 
         $(logTM) ErrorS (logStr (show e)) $> 
         Error (ServerError (InternalServerError (show e^.stextl)))
    either mkErr (return . Fortune . GetTrajectoriesResponse . Response . (^.vector)) x

action :: UserId -> KatipLoggerIO -> Hasql.Session.Session [Trajectory]
action uid logger = 
  do 
    let sql = 
         [i|select 
             qp."qualificationProviderTitle",
             qp."qualificationProviderDegreeType",
             qpf.language, pr."providerTitle",
             qpf."admissionDeadline", tr.overlap
            from "edgeNode"."Trajectory" as tr 
            join "edgeNode"."QualificationProvider" as qp
              on tr."qualificationKey" = qp.id
            join "edgeNode"."QualificationProviderFeatures" as qpf
              on qpf."qualificationKey" = qp.id  
            join "edgeNode"."Provider" as pr 
              on qp."qualificationProviderKey" = pr.id 
            where tr.user = $1|] 
    let encoder = uid^._Wrapped' >$ HE.param HE.int8
    let trajectoryDecoder = 
         do trajectoryQualificationTitle <- 
              HD.column HD.text <&> (^.from lazytext)
            trajectoryQualificationDegreeType <- 
              HD.column HD.text <&> (^.from lazytext)
            trajectoryTuitionLanguage <- 
              HD.column HD.text <&> 
              (Enumerated . Right . (^.from stext.from isoLanguage))
            trajectoryProviderTitle <- 
              HD.column HD.text  <&> (^.from lazytext)
            trajectoryAdmissionDeadline <- 
              HD.column HD.timestamptz <&>
              (Just . (`Time` 0) . fromIntegral . round . utcTimeToPOSIXSeconds)
            trajectoryMatch <- 
              HD.column 
              (HD.jsonbBytes 
              ( first (^.stext) 
              . Aeson.eitherDecodeStrict))
            return Trajectory {..}
    let decoder = HD.rowList trajectoryDecoder
    liftIO $ logger DebugS (logStr (sql^.from textbs.from stext))
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)