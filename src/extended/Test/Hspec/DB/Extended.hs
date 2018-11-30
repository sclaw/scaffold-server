{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Test.Hspec.DB.Extended
       (  module Test.Hspec.DB
        , itDBGroundhog
        , describeDBGroundhog
       )
       where

import Test.Hspec.DB hiding (pool)
import Database.Groundhog.Core (Action, Migration, runDbConn)
import Test.Hspec
import qualified Database.Postgres.Temp as Temp
import Database.Groundhog.Postgresql (Postgresql, createPostgresqlPool, withPostgresqlConn)
import Data.Pool (destroyAllResources, Pool)
import Control.Exception (throwIO)
import Database.Groundhog (runMigration)
import Control.Monad (void)


data TestDBPGSql =
     TestDBPGSql
      {   -- handle for temporary @postgres@ process
          tempDB :: Temp.DB
          -- pool of connections to the temporary @postgres@
         , pool  :: Pool Postgresql
      }

-- start a temporary postgres process and create a pool of connections to it
-- start a temporary postgres process and create a pool of connections to it
setupDBGroundhog :: Migration (Action Postgresql) -> Action Postgresql () -> IO TestDBPGSql
setupDBGroundhog migration populate =
    do
      e <- Temp.start []
      tempDB <- either throwIO return e
      let connStr = Temp.connectionString tempDB
      putStrLn $ "connection to tmp psql set up: " ++ connStr
      pool <- connStr `createPostgresqlPool` 10
      let prepare = runMigration migration >> populate            
      withPostgresqlConn connStr (prepare `runDbConn`)
      return TestDBPGSql {..}

-- drop all the connections and shutdown the postgres process
teardownDBGroundhog :: TestDBPGSql -> IO ()
teardownDBGroundhog TestDBPGSql {..} =
    do
      destroyAllResources pool
      void $ Temp.stop tempDB

withDBGroundhog :: Action Postgresql a -> TestDBPGSql -> IO a
withDBGroundhog action db = action `runDbConn` pool db

-- dlipped version of 'withDB'
runDBGroundhog :: TestDBPGSql -> Action Postgresql a -> IO a
runDBGroundhog = flip withDBGroundhog

itDBGroundhog :: String -> Action Postgresql a -> SpecWith TestDBPGSql
itDBGroundhog msg action = msg `it` (void . withDBGroundhog action)

describeDBGroundhog :: Migration (Action Postgresql) -> String -> SpecWith TestDBPGSql -> Spec
describeDBGroundhog  migrate discribeTest =
      beforeAll (setupDBGroundhog migrate (return ()))
    . afterAll teardownDBGroundhog
    . describe discribeTest

describeDBGroundhogWithTablePopulated 
       :: Migration (Action Postgresql) 
       -> Action Postgresql () 
       -> String 
       -> SpecWith TestDBPGSql -> Spec
describeDBGroundhogWithTablePopulated migrate populate discribeTest =
      beforeAll (setupDBGroundhog migrate populate)
    . afterAll teardownDBGroundhog
    . describe discribeTest