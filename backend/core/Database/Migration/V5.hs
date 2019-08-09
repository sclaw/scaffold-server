{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.V5 (sql) where

import EdgeNode.Model.EducationLevel
import EdgeNode.Model.User.Qualification

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|drop table if exists "edgeNode"."userQualification";
         drop table if exists "edgeNode"."#{show (typeOf (undefined :: Qualification))}";
         create table if not exists "edgeNode"."#{show (typeOf (undefined :: StateExam))}" 
         ("stateExamTitle" text not null, "stateExamCountry" text not null);
         create table if not exists "edgeNode"."#{show (typeOf (undefined :: HigherDegree))}" 
         ("higherDegreeCountry" text not null, "higherDegreeProvider" text not null);
         create table if not exists "edgeNode"."#{show (typeOf (undefined :: InternationalDiploma))}" 
         ("internationalDiplomaTitle" text not null);
         create table if not exists "edgeNode"."#{show (typeOf (undefined :: LanguageStandard))}" 
         ("languageStandardStandard" text not null, "languageStandardGrade" bytea not null);
      |]