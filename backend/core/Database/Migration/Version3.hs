{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version3 (sql) where

import EdgeNode.Model.Token (Token)

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|alter table "auth"."#{show (typeOf (undefined :: Token))}"
         drop constraint "token_tokenUserId_uk";
         alter table "auth"."#{show (typeOf (undefined :: Token))}"
         add column "tokenUnique" int8 not null default floor(random()*(10000-1+1))+1;
         alter table "auth"."#{show (typeOf (undefined :: Token))}"
         add constraint "token_tokenUserId_tokenUnique_uk" unique("tokenUserId", "tokenUnique")
      |]