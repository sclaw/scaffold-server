{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version5 (sql) where

import EdgeNode.Model.Category

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|create table if not exists "edgeNode"."#{show (typeOf (undefined :: StateExam))}" 
         (id bigserial primary key, 
          "stateExamTitle" text not null, 
          "stateExamCountry" text not null);
         create table if not exists "edgeNode"."#{show (typeOf (undefined :: HigherDegree))}" 
         (id bigserial primary key, 
          "higherDegreeCountry" text not null, 
          "higherDegreeProvider" text not null);
         create table if not exists "edgeNode"."#{show (typeOf (undefined :: InternationalDiploma))}" 
         (id bigserial primary key, 
          "internationalDiplomaTitle" text not null);
         create table if not exists "edgeNode"."#{show (typeOf (undefined :: LanguageStandard))}" 
         (id bigserial primary key, 
          "languageStandardStandard" text not null, 
          "languageStandardGrade" bytea not null);
      |]