{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.V2 (sql) where

import EdgeNode.Model.User.Qualification (StateExamination)
import EdgeNode.Model.User (User)

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|create table "edgeNode"."#{show (typeOf (undefined :: StateExamination))}" 
         (id bigserial not null constraint "StateExamination_pk" primary key,
          country bytea not null, name bytea not null,
          provider bytea not null);
         create table "edgeNode"."userQualification"
         (user_id int8 not null constraint "userQualification_user_id_fk" references "edgeNode"."User"(id),
          "stateExamination_id" int8 not null constraint 
          "userQualification_stateExamination_id_fk" 
          references "edgeNode"."StateExamination"(id));
         alter table "edgeNode"."#{show (typeOf (undefined :: User))}" add column "userGender" bytea null 
      |]