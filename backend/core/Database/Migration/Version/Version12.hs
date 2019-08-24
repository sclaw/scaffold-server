{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version12 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|alter table "edgeNode"."QualificationDependency"
         drop column "dependency";
         alter table "edgeNode"."QualificationDependency" 
         add column  "dependency" int8 not null 
         constraint "#QualificationDependency_dependency_fk" 
         references "edgeNode"."QualificationProvider"(id);|]