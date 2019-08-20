{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version10 (sql) where

import EdgeNode.Model.Qualification
import EdgeNode.Model.User.Qualification

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|alter table "edgeNode"."#{show (typeOf (undefined :: QualificationProvider))}"  
         rename "qualificationProviderGrade" 
         to "qualificationProviderGradeRange";
         alter table "edgeNode"."#{show (typeOf (undefined :: QualificationProvider))}"
         add column "minRequiredGrade" jsonb;
         create table if not exists "edgeNode"."#{show (typeOf (undefined :: QualificationDependencies))}"
         ("key" int8 not null 
          constraint "#{show (typeOf (undefined :: QualificationDependencies))}_key_fk" 
          references "edgeNode"."#{show (typeOf (undefined :: QualificationProvider))}"(id),
          "dependency" int8 not null 
          constraint "#{show (typeOf (undefined :: QualificationProvider))}_dependency_fk" 
          references "edgeNode"."#{show (typeOf (undefined :: QualificationProvider))}"(id),
          constraint "#{show (typeOf (undefined :: QualificationDependencies))}_key_dependency_uniq" 
          unique ("key", "dependency"));
         alter table "edgeNode"."#{show (typeOf (undefined :: UserQualification))}" add column 
         "qualificationSkillLevel" jsonb; 
      |]