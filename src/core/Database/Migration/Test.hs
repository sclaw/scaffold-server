{-# LANGUAGE TemplateHaskell #-}

module Database.Migration.Test (migrate) where

import Hasql.Statement
import TH.Mk
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Data.ByteString as B

$mkMigrationTest

migrate :: [Statement () ()]
migrate = schemas ++ (flip concatMap list $ \sql -> if B.null sql then [] else [Statement sql HE.unit HD.unit False])
  where 
    schemas =
      [ Statement "create extension postgres_fdw" HE.unit HD.unit False
      , Statement "create extension hstore" HE.unit HD.unit False
      , Statement "create extension ltree" HE.unit HD.unit False
      , Statement "create extension pg_trgm" HE.unit HD.unit False
      , Statement "create schema auth" HE.unit HD.unit False
      , Statement "create schema \"edgeNode\"" HE.unit HD.unit False
      ]