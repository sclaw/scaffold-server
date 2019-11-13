{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Migration.Test (migrate) where

import Hasql.Statement
import TH.Mk
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD

$mkMigrationTest

migrate :: [Statement () ()]
migrate = exts ++ map (\sql -> Statement sql HE.unit HD.unit False) list
  where 
    exts =
      [ Statement "create extension postgres_fdw" HE.unit HD.unit False
      , Statement "create extension hstore" HE.unit HD.unit False
      , Statement "create extension ltree" HE.unit HD.unit False
      , Statement "create extension pg_trgm" HE.unit HD.unit False
      ]