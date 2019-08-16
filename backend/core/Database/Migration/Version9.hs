{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version9 (sql) where

import EdgeNode.Model.User.Qualification
import EdgeNode.Model.Provider
import EdgeNode.Model.Qualification

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|create table if not exists "edgeNode"."#{show (typeOf (undefined :: UserQualification))}"
         (id bigserial primary key, 
          "categoryType" text not null,
          "categoryKey" int8 not null, 
          "providerKey" int8 not null 
          constraint "#{show (typeOf (undefined :: UserQualification))}_provider_id_fk" 
          references "edgeNode"."#{show (typeOf (undefined :: Provider))}"(id),
          "qualificationKey" int8 not null 
          constraint "#{show (typeOf (undefined :: UserQualification))}_qualification_id_fk" 
          references "edgeNode"."#{show (typeOf (undefined :: QualificationProvider))}"(id),
          constraint "#{show (typeOf (undefined :: UserQualification))}_categoryType_categoryKey_providerKey_qualificationKey_uniq" 
          unique ("categoryType", "categoryKey", "providerKey", "qualificationKey"));
      |]