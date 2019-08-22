{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version11 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|alter table "edgeNode"."QualificationProvider"  
         drop column "minRequiredGrade"; 
         alter table "edgeNode"."QualificationDependencies" 
         rename to "QualificationDependency";
         alter table "edgeNode"."QualificationDependency"
         drop column "dependency";
         alter table "edgeNode"."QualificationDependency" 
         add column  "dependency" int8 not null 
         constraint "#QualificationDependency_dependency_fk" 
         references "QualificationProvider"(id);
         alter table "edgeNode"."QualificationDependency" 
         add column "minRequiredGrade" jsonb;
         alter table "edgeNode"."UserQualification" 
         rename constraint "User_provider_id_fk" to "UserQualification_userId_fk";|]