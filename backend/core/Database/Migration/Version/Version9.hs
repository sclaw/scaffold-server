{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version9 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|create table if not exists "edgeNode"."UserQualification"
         (id bigserial primary key, 
          "userId" int8 not null constraint "UserQualification_userId_fk"
          references "edgeNode"."User"(id) ,
          "categoryType" text not null,
          "categoryKey" int8 not null, 
          "providerKey" int8 not null 
          constraint "UserQualification_providerKey_fk" 
          references "edgeNode"."Provider"(id),
          "qualificationKey" int8 not null 
          constraint "UserQualification_qualificationKey_fk" 
          references "edgeNode"."QualificationProvider"(id),
          constraint "UserQualification_categoryType_categoryKey_providerKey_qualificationKey_uniq" 
          unique ("categoryType", "categoryKey", "providerKey", "qualificationKey"));|]