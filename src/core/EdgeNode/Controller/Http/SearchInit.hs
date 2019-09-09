{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Http.SearchInit (controller) where

import EdgeNode.Provider.Qualification
import EdgeNode.Error
import EdgeNode.Model.Qualification ()

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
import qualified Hasql.Statement as HS 
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.String.Interpolate
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Time.Time

controller :: KatipController (Alternative (Error T.Text) [XQualificationFullInfo])
controller = 
  do
    raw <- (^.katipEnv.rawDB) `fmap` ask
    x <- runTryDbConnHasql (const action) raw
    whenLeft x ($(logTM) ErrorS . logStr . show) 
    let mkErr e = ServerError $ InternalServerError (show e^.stextl)     
    return $ first mkErr x^.eitherToAlt

action :: Hasql.Session.Session [XQualificationFullInfo]    
action = 
  do
    let sql = 
         [i|select qp.id, qp."qualificationProviderTitle",
            qp."qualificationProviderDegreeType",
            pr."providerTitle",
            qpf."durationPeriod",
            qpf."tuitionFees",
            qpf."admissionDeadline",
            qpf."studyMode"
            from "edgeNode"."QualificationProvider" as qp
            join "edgeNode"."Provider" as pr
            on pr.id = qp."qualificationProviderKey"
            join "edgeNode"."QualificationProviderFeatures" as qpf 
            on qpf."qualificationKey" = qp.id|]
    let qualificationDecoder = 
          do
            ident <- HD.column HD.int8 <&> (^._Unwrapped')
            qualificationFullInfoTitle <- HD.column HD.text <&> (^.from lazytext)
            qualificationFullInfoDegreeType <- HD.column HD.text <&> (^.from lazytext)
            qualificationFullInfoProvider <- HD.column HD.text <&> (^.from lazytext)
            qualificationFullInfoDuration <- HD.column HD.int4
            qualificationFullInfoTuitionFees <- HD.column HD.int4
            let mkTime day = Just $ Time ((fromInteger . round . utcTimeToPOSIXSeconds . (`UTCTime` 0)) day) 0
            qualificationFullInfoAdmissionDeadline <- HD.column HD.date <&> mkTime
            qualificationFullInfoStudyMode <- HD.column HD.text <&> (^.from lazytext)
            let value = QualificationFullInfo {..}
            return $ XQualificationFullInfo (Just ident) (Just value)
    let decoder = HD.rowList qualificationDecoder
    Hasql.Session.statement () (HS.Statement sql HE.unit decoder False)