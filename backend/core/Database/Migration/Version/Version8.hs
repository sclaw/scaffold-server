{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version8 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|create table if not exists "edgeNode"."QualificationProvider" 
         (id bigserial primary key,
          "qualificationProviderKey" int8 not null 
          constraint "QualificationProvider_provider_id_fk" 
          references "edgeNode"."Provider"(id),
          "qualificationProviderDegreeType" text,
          "qualificationProviderTitle" text not null,
          "qualificationProviderGrade" jsonb, path ltree,
          constraint "QualificationProvider_qualificationProviderTitle_path_uniq" 
          unique ("qualificationProviderTitle", path));
          create index "QualificationProvider_path_idx" on 
          "edgeNode"."QualificationProvider" 
          using gist (path);|]