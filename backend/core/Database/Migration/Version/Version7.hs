{-# LANGUAGE QuasiQuotes #-}

module Database.Migration.Version.Version7 (sql) where

import Database.FullText.Index
import Data.String.Interpolate

sql :: String 
sql = [i|alter table "edgeNode"."Provider" add column if not exists "providerCountry" text not null;
         #{concatMap (\(x, y) -> x ++ ";" ++ y) $ recreateIndex providerDropIndexQuery providerCreateIndexQuery}|]