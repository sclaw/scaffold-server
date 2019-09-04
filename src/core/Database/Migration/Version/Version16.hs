{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version16 (sql) where

import Data.String.Interpolate

sql :: String
sql = [i|alter table "edgeNode"."QualificationProvider" 
         add column "qualificationProviderCategoryType" text;
         update "edgeNode"."QualificationProvider" 
          set "qualificationProviderCategoryType" = 'state_exam' 
         where "qualificationProviderDegreeType" = 
          any(array['UnifiedStateExam', 'ALevelGCE']);
         update "edgeNode"."QualificationProvider" 
          set "qualificationProviderCategoryType" = 'higher_degree' 
         where not ("qualificationProviderDegreeType" = 
          any(array['UnifiedStateExam', 'ALevelGCE']));
         alter table "edgeNode"."QualificationProvider";
         alter table "edgeNode"."Trajectory" 
         rename column "qualififcationKey" to "qualificationKey";
         alter table "edgeNode"."Trajectory" 
         add column id serial primary key;|]