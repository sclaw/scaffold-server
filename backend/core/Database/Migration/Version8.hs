{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version8 (sql) where

import EdgeNode.Model.Qualification

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|create table if not exists "edgeNode"."#{show (typeOf (undefined :: QualificationProvider))}" 
         (id bigserial primary key,
          "qualificationProviderKey" int8 not null 
          constraint "QualificationProvider_provider_id_fk" 
          references "edgeNode"."Provider"(id),
          "qualificationProviderDegreeType" text,
          "qualificationProviderTitle" text not null,
          "qualificationProviderGrade" jsonb, path ltree,
          constraint "#{show (typeOf (undefined :: QualificationProvider))}_qualificationProviderTitle_path_uniq" 
          unique ("qualificationProviderTitle", path));
         create index "QualificationProvider_path_idx" on 
         "edgeNode"."#{show (typeOf (undefined :: QualificationProvider))}" 
         using gist (path);
      |]