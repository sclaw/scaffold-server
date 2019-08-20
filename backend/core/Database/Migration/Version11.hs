{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version11 (sql) where

import EdgeNode.Model.Qualification

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|alter table "edgeNode"."#{show (typeOf (undefined :: QualificationProvider))}"  
         drop column "minRequiredGrade"; 
         alter table "edgeNode"."QualificationDependencies" 
         rename to "QualificationDependency";
         alter table "edgeNode"."QualificationDependency" 
         add column "minRequiredGrade" jsonb;
      |]