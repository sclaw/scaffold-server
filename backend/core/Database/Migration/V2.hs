{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.V2 (sql) where

import EdgeNode.Model.User.Qualification (StateExamination)
import EdgeNode.Model.User (User)

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|create table "edgeNode"."#{show (typeOf (undefined :: StateExamination))}" (test int not null);
         alter table "edgeNode"."#{show (typeOf (undefined :: User))}" add column "userGender" bytea null 
      |]