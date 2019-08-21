{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version4 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|alter table "auth"."Token" drop column "tokenUnique";
         alter table "auth"."Token" add column "tokenUnique" text;|]