{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.V4 (sql) where

import EdgeNode.Model.Token (Token)

import Data.String.Interpolate
import Data.Typeable

sql :: String 
sql = [i|alter table "auth"."#{show (typeOf (undefined :: Token))}" drop column "tokenUnique";
         alter table "auth"."#{show (typeOf (undefined :: Token))}" add column "tokenUnique" text
      |]