{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version10 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|alter table "edgeNode"."QualificationProvider"  
         rename "qualificationProviderGrade" 
         to "qualificationProviderGradeRange";
         alter table "edgeNode"."QualificationProvider"
         add column "minRequiredGrade" jsonb;
         create table if not exists "edgeNode"."QualificationDependencies"
         ("key" int8 not null 
          constraint "QualificationDependencies_key_fk" 
          references "edgeNode"."QualificationProvide"(id),
          "dependency" int8 not null 
          constraint "QualificationProvider_dependency_fk" 
          references "edgeNode"."QualificationProvider"(id),
          constraint "QualificationDependencies_key_dependency_uniq" 
          unique ("key", "dependency"));
         alter table "edgeNode"."UserQualification" add column 
         "qualificationSkillLevel" jsonb; 
      |]