{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version15 (sql) where

import Data.String.Interpolate
import RetrofitProto
import EdgeNode.Lang

sql :: String
sql = [i|alter table "edgeNode"."QualificationProviderFeatures" 
         rename column "qualififcationKey" to "qualificationKey";
         insert into "edgeNode"."QualificationProviderFeatures"
         ("qualificationKey", "academicAreas", "durationPeriod",
          "tuitionFees", "admissionDeadline", "language", "studyMode")
         values (7, '["Engineering", "Computing"]', 1, null, date '2019-06-14',
         '#{fromLanguage LanguageEnglish}', 'Full-time');
      |]