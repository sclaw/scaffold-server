{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Test.Hspec.DB.Extended
       (  module Test.Hspec.DB
        , itDBGroundhog
        , describeDBGroundhog
        , describeDBGroundhogWithTablePopulated
       )
       where

import           Control.Exception             (bracketOnError)
import           Control.Monad                 (void)
import           Data.Pool                     (Pool, destroyAllResources)
import           Database.Groundhog.Core       (Action, Migration, runDbConn)
import           Database.Groundhog.Generic    (runMigrationSilent)
import           Database.Groundhog.Postgresql (Postgresql, createPostgresqlPool,
                                                withPostgresqlConn)
import qualified Database.Postgres.Temp        as Temp
import           Test.Hspec
import           Test.Hspec.DB                 hiding (pool)


data TestDBPGSql =
     TestDBPGSql
      {   -- handle for temporary @postgres@ process
          tempDB :: Temp.DB
          -- pool of connections to the temporary @postgres@
         , pool  :: Pool Postgresql
      }

-- start a temporary postgres process and create a pool of connections to it
setupDBGroundhog :: Migration (Action Postgresql) -> Action Postgresql () -> IO TestDBPGSql
setupDBGroundhog migration populate =
    bracketOnError
    (Temp.startAndLogToTmp [] >>=
     either err return)
    (void . Temp.stop)
    initTemp
    where
        initTemp tempDB =
         do let connStr = Temp.connectionString tempDB
            pool <- connStr `createPostgresqlPool` 10
            let prepare =
                 do
                   runMigrationSilent
                    migration
                   populate
            withPostgresqlConn connStr (prepare `runDbConn`)
            return TestDBPGSql {..}
        err e = error $ "Error during db initialization: " <> show e

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

describeDBGroundhog
    :: Migration (Action Postgresql)
    -> String
    -> SpecWith TestDBPGSql
    -> Spec
describeDBGroundhog migrate str =
      beforeAll
      (setupDBGroundhog
       migrate
       (return ())
      )
    . afterAll teardownDBGroundhog
    . describe str

describeDBGroundhogWithTablePopulated
    :: Migration (Action Postgresql)
    -> Action Postgresql ()
    -> String
    -> SpecWith TestDBPGSql
    -> Spec
describeDBGroundhogWithTablePopulated migrate populate str =
      beforeAll
      (setupDBGroundhog
       migrate
       populate
      )
      . afterAll
        teardownDBGroundhog
      . describe str