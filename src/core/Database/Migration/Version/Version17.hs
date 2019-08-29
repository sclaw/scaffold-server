{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version17 (sql) where

import Data.String.Interpolate

sql :: String
sql = [i|alter table "edgeNode"."UserQualification"
         drop constraint "UserQualification_categoryType_categoryKey_providerKey_qualificationKey_uniq";
         alter table "edgeNode"."UserQualification" 
         add constraint "UserQualification_userId_categoryType_categoryKey_providerKey_qualificationKey_uniq"
         unique ("userId", "categoryType", "categoryKey", "providerKey", "qualificationKey");|]