{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version13 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|create table if not exists "edgeNode"."QualificationProviderFeatures" 
         ("qualififcationKey" int8 not null 
          constraint "QualificationProviderFeatures_qualififcationKey_fk" 
          references "edgeNode"."QualificationProvider"(id),
          "academicAreas" jsonb, "durationPeriod" int, 
          "tuitionFees" int, "admissionDeadline" date, 
          "language" text not null, "studyMode" text,
          constraint "QualificationProviderFeatures_qualififcationKey_uniq" 
          unique ("qualififcationKey"));
         create table if not exists "edgeNode"."Trajectory" 
         ("user" int8 not null
          constraint "Trajectory_user_fk" 
          references "edgeNode"."User"(id),           
          "qualififcationKey" int8 not null 
          constraint "Trajectory_qualififcationKey_fk" 
          references "edgeNode"."QualificationProvider"(id),
          "overlap" jsonb not null,
          constraint "Trajectory_qualififcationKey_uniq" 
          unique ("qualififcationKey", "user"));|]