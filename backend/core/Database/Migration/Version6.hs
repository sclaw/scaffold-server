{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version6 (sql) where

import EdgeNode.Model.Category

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|alter table "edgeNode"."#{show (typeOf (undefined :: LanguageStandard))}" 
         drop column "languageStandardGrade";
         alter table "edgeNode"."#{show (typeOf (undefined :: LanguageStandard))}" 
         add column "languageStandardGrade" jsonb not null 
         default '{"intgeral":0,"fractional":0}';
         create table if not exists "edgeNode"."Provider"
         (id bigserial primary key, "providerTitle" text not null);   
         create table if not exists "edgeNode"."StateExamProvider"
         (state_exam_id int8 not null 
          constraint "StateExamProvider_state_exam_id_fk" 
          references "edgeNode"."StateExam"(id),
          provider_id int8 not null 
          constraint "StateExamProvider_provider_id_fk" 
          references "edgeNode"."Provider"(id));
         create table if not exists "edgeNode"."HigherDegreeProvider"
         (higher_degree_id int8 not null constraint "HigherDegreeProvider_higher_degree_id_fk" 
          references "edgeNode"."HigherDegree"(id),
          provider_id int8 not null constraint "HigherDegreeProvider_provider_id_fk" 
          references "edgeNode"."Provider"(id));
         create table if not exists "edgeNode"."InternationalDiplomaProvider"
         (international_diploma_id int8 not null 
          constraint "InternationalDiplomaProvider_international_diploma_id_fk" 
          references "edgeNode"."InternationalDiploma"(id),
          provider_id int8 not null constraint "InternationalDiplomaProvider_provider_id_fk" 
          references "edgeNode"."Provider"(id));
         create table if not exists "edgeNode"."LanguageStandardProvider"
         (language_standard_id int8 not null 
          constraint "LanguageStandardProvider_language_standard_id_fk" 
          references "edgeNode"."LanguageStandard"(id),
          provider_id int8 not null constraint "LanguageStandardProvider_provider_id_fk" 
          references "edgeNode"."Provider"(id));                                
      |]