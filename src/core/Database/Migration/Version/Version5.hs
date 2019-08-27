{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version5 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|create table if not exists "edgeNode"."StateExam" 
         (id bigserial primary key, 
          "stateExamTitle" text not null, 
          "stateExamCountry" text not null);
         create table if not exists "edgeNode"."HigherDegree" 
         (id bigserial primary key, 
          "higherDegreeCountry" text not null, 
          "higherDegreeProvider" text not null);
         create table if not exists "edgeNode"."InternationalDiploma" 
         (id bigserial primary key, 
          "internationalDiplomaTitle" text not null);
         create table if not exists "edgeNode"."LanguageStandard" 
         (id bigserial primary key, 
          "languageStandardStandard" text not null, 
          "languageStandardGrade" bytea not null);|]