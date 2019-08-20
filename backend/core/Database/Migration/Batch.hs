{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Database.Migration.Batch (Version (..), exec) where 

import EdgeNode.Application
import qualified Database.Migration.Version2 as V2
import qualified Database.Migration.Version3 as V3
import qualified Database.Migration.Version4 as V4
import qualified Database.Migration.Version5 as V5
import qualified Database.Migration.Version6 as V6
import qualified Database.Migration.Version7 as V7
import qualified Database.Migration.Version8 as V8
import qualified Database.Migration.Version9 as V9
import qualified Database.Migration.Version10 as V10
import qualified Database.Migration.Version11 as V11

import Data.Word (Word32)
import Database.Exception
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Katip
import Control.Monad.Except
import Data.Coerce
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Data.Sort
import Control.Lens

newtype Version = Version Word32
  deriving newtype Num
  deriving newtype Eq
  deriving newtype Ord
  deriving stock Show 

data MigrationStep = 
       NextSql String Version 
     | NextMigration (Migration (TryAction Groundhog (KatipContextT AppMonad) Postgresql)) Version    
     | Stop

exec :: Version -> TryAction Groundhog (KatipContextT AppMonad) Postgresql (Maybe Version) 
exec _ | null list = return Nothing    
exec ver = maybe err ok (migrMap Map.!? ver) 
  where
    ok val | isEmpty val = throwError (MigrationSqlEmpty (coerce (ver + 1)))  
    ok val = 
      case val of
        Stop -> return (Just ver)
        NextSql sql ver -> withSql sql >> exec ver
        NextMigration migr ver -> 
          mkSql migr >>= mapM_ withSql >> exec ver
    withSql sql = 
      do 
        $(logTM) InfoS (logStr ([i|new migration from #{ver} to #{ver + 1}|] :: String))
        $(logTM) InfoS (logStr ([i|query: #{sql}|] :: String))
        executeRaw False sql []     
    err = throwError (MigrationNotFound (coerce ver))     
    migrMap = Map.fromList list

isEmpty :: MigrationStep -> Bool
isEmpty (NextSql sql _) = null sql
isEmpty (NextMigration _ _) = False
isEmpty Stop = False

mkSql 
  :: Migration (TryAction Groundhog (KatipContextT AppMonad) Postgresql)
  -> TryAction Groundhog (KatipContextT AppMonad) Postgresql [String]
mkSql migration = createMigration migration >>= (fmap concat . make)
  where 
   make migs = forM (Map.assocs migs) $ \(_, v) -> either (fmap (const []) . err) (return . ok) v
   err = mapM_ (\e -> $(logTM) InfoS (logStr ("\tError:\t" ++ e)))
   ok xs = map (^._3) $ sortOn (^._2) xs
         
list :: [(Version, MigrationStep)]
list = 
  [ (Version 1, NextSql V2.sql (Version 2))
  , (Version 2, NextSql V3.sql (Version 3))
  , (Version 3, NextSql V4.sql (Version 4))
  , (Version 4, NextSql V5.sql (Version 5))
  , (Version 5, NextSql V6.sql (Version 6))
  , (Version 6, NextSql V7.sql (Version 7))
  , (Version 7, NextSql V8.sql (Version 8))
  , (Version 8, NextSql V9.sql (Version 9))
  , (Version 9, NextSql V10.sql (Version 10))
  , (Version 10, NextSql V11.sql (Version 11))
  , (Version 11, Stop)
  ]