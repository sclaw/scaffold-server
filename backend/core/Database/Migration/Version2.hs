{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version2 (sql) where

import EdgeNode.Model.User (User)

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i| (id bigserial not null constraint "Qualification_pk" primary key,
          country varchar not null, name varchar not null,
          provider varchar not null);
         create table if not exists "edgeNode"."userQualification"
         (user_id int8 not null constraint "userQualification_user_id_fk" references "edgeNode"."User"(id),
          "qualification_id" int8 not null constraint 
          "userQualification_qualification_id_fk" 
          references "edgeNode"."Qualification"(id));
         alter table "edgeNode"."#{show (typeOf (undefined :: User))}" add column if not exists "userGender" varchar null 
      |]