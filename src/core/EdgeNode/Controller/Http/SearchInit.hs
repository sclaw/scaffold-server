{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Http.SearchInit (controller, action) where

import EdgeNode.Provider.Qualification
import EdgeNode.Model.Qualification ()
import EdgeNode.Model.User
import EdgeNode.Search.Filter
import EdgeNode.Iso

import KatipController
import Json
import Database.Transaction
import qualified Data.Text as T
import Control.Lens.Iso.Extended
import Control.Lens
import qualified Hasql.Session as Hasql.Session
import qualified Hasql.Statement as HS 
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.String.Interpolate
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Time.Time
import Protobuf.Scalar
import Proto3.Suite.Types
import TH.Proto
import Contravariant.Extras.Contrazip
import Data.Foldable

controller :: Maybe UserId -> KatipController (Alternative (Error T.Text) [XQualificationFullInfo])
controller ident = 
  do
    hasql <- (^.katipEnv.hasqlDbPool) `fmap` ask
    fmap Fortune $ katipTransaction hasql $ lift $ action ident Nothing

action :: Maybe UserId -> Maybe Filter -> Hasql.Session.Session [XQualificationFullInfo]    
action ident filter = 
  do
    let sql = 
         [i|select 
            qp.id, 
            qp."qualificationProviderTitle",
            qp."qualificationProviderDegreeType",
            pr."providerTitle",
            pr."providerCountry",
            qpf."durationPeriod",
            qpf."tuitionFeesPerAnnum",
            qpf."admissionDeadline",
            qpf."studyMode",
            (exists (
             select 1 
             from "edgeNode"."Trajectory" 
             where "qualificationKey" = qp.id 
                   and "user" = $1))        
            from "edgeNode"."Provider" as pr
            left join "edgeNode"."QualificationProvider" as qp
            on pr.id = qp."qualificationProviderKey"
            left join "edgeNode"."QualificationProviderFeatures" as qpf 
            on qpf."qualificationKey" = qp.id
            where "qualificationProviderDegreeType" = 'Bakalavr' and
                  (case when $2::text[] is not null  
                        then (select exists (select 1 from 
                              (select * from jsonb_array_elements_text(qpf."academicAreas"::jsonb) as t(v)
                               intersect
                               select v from unnest($2::text[]) as t(v)) as v))
                        else true end) 
                  and
                  (case when $3::text[] is not null 
                        then qpf."language" = any($3) 
                        else true end)
                  and      
                  (case when $4::text[] is not null 
                        then pr."providerCountry" = any($4) 
                        else true end)
                  and
                  (case when $5::text[] is not null 
                        then qp."qualificationProviderDegreeType" = any($5) 
                        else true end)
            order by pr."providerCountry", qp."qualificationProviderTitle"|]
    let mkTpl (Just Filter {..}) = 
          ( fmap mkAreas filterAcademicAreas
          , fmap mkLangs filterLanguages
          , fmap mkCntrs filterCountries
          , fmap mkdDegrees filterDegrees)  
        mkTpl Nothing = (Nothing, Nothing, Nothing, Nothing)        
    let encoder = 
         (ident^?_Just._Wrapped' >$ HE.param (HE.nullable HE.int8)) <>
         (mkTpl filter >$ 
          (contrazip4 
           (HE.param (HE.nullable (HE.array (HE.dimension foldl' (HE.element (HE.nonNullable HE.text))))))
           (HE.param (HE.nullable (HE.array (HE.dimension foldl' (HE.element (HE.nonNullable HE.text))))))
           (HE.param (HE.nullable (HE.array (HE.dimension foldl' (HE.element (HE.nonNullable HE.text))))))
           (HE.param (HE.nullable (HE.array (HE.dimension foldl' (HE.element (HE.nonNullable HE.text))))))))      
    let qualificationDecoder = 
          do
            ident <- HD.column (HD.nonNullable HD.int8) <&> (^._Unwrapped')
            qualificationFullInfoTitle <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
            qualificationFullInfoDegreeType <- HD.column (HD.nullable HD.text) <&> fmap (^.from lazytext.to String)
            qualificationFullInfoProvider <- HD.column (HD.nonNullable HD.text) <&> (^.from lazytext)
            qualificationFullInfoProviderCountry <- 
              HD.column (HD.nonNullable (HD.enum (Just . (^.from stext.to (Enumerated . Right . toCountry)))))
            qualificationFullInfoDuration <- HD.column (HD.nullable HD.int4) <&> fmap Int32
            qualificationFullInfoTuitionFees <- HD.column (HD.nullable HD.int4) <&> fmap Int32
            let mkTime day = Time ((fromInteger . round . utcTimeToPOSIXSeconds . (`UTCTime` 0)) day) 0
            qualificationFullInfoAdmissionDeadline <- HD.column (HD.nullable HD.date) <&> fmap mkTime
            qualificationFullInfoStudyMode <- HD.column (HD.nullable HD.text) <&> fmap (^.from lazytext.to String)
            isTrajectory <- HD.column (HD.nonNullable HD.bool)
            let value = QualificationFullInfo {..}
            return $ XQualificationFullInfo (Just ident) (Just value) isTrajectory
    let decoder = HD.rowList qualificationDecoder
    Hasql.Session.statement () (HS.Statement sql encoder decoder False)

mkAreas :: AcademicAreas -> [T.Text]
mkAreas (AcademicAreas v) = v^..traversed.lazytext

mkLangs :: Languages -> [T.Text]
mkLangs (Languages v) = v^..traversed.language

mkCntrs :: Countries -> [T.Text]
mkCntrs (Countries v) = v^..traversed.country

mkdDegrees :: Degrees -> [T.Text]
mkdDegrees (Degrees v) = v^..traversed.lazytext