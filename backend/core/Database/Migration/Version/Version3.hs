{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version3 (sql) where

import Data.String.Interpolate

sql :: String 
sql = [i|alter table "auth"."Token"
         drop constraint "token_tokenUserId_uk";
         alter table "auth"."Token"
         add column "tokenUnique" int8 not null default floor(random()*(10000-1+1))+1;
         alter table "auth"."Token"
         add constraint "token_tokenUserId_tokenUnique_uk" 
         unique("tokenUserId", "tokenUnique");|]