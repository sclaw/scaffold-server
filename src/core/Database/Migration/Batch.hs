{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
import qualified Database.Migration.Version.Version2 as V2
import qualified Database.Migration.Version.Version3 as V3
import qualified Database.Migration.Version.Version4 as V4
import qualified Database.Migration.Version.Version5 as V5
import qualified Database.Migration.Version.Version6 as V6
import qualified Database.Migration.Version.Version7 as V7
import qualified Database.Migration.Version.Version8 as V8
import qualified Database.Migration.Version.Version9 as V9
import qualified Database.Migration.Version.Version10 as V10
import qualified Database.Migration.Version.Version11 as V11
import qualified Database.Migration.Version.Version12 as V12
import qualified Database.Migration.Version.Version13 as V13
import qualified Database.Migration.Version.Version14 as V14
import qualified Database.Migration.Version.Version15 as V15
import qualified Database.Migration.Version.Version16 as V16
import qualified Database.Migration.Version.Version17 as V17
import qualified Database.Migration.Version.Version18 as V18

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
import TH.Mk

newtype Version = Version Word32
  deriving newtype Num
  deriving newtype Eq
  deriving newtype Ord
  deriving stock Show 

data MigrationStep = 
       NextSql String Version 
     | NextMigration 
       (Migration 
        (TryAction (Groundhog ())
         (KatipContextT AppMonad) 
          Postgresql)) 
       Version    
     | Stop

$(mkMigrationSeq 1 18)

exec :: Version -> TryAction (Groundhog ()) (KatipContextT AppMonad) Postgresql (Maybe Version) 
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
  :: Migration (TryAction (Groundhog ()) (KatipContextT AppMonad) Postgresql)
  -> TryAction (Groundhog ()) (KatipContextT AppMonad) Postgresql [String]
mkSql migration = 
  createMigration migration >>= 
  (fmap concat . make)
  where 
   make migs = 
     forM (Map.assocs migs) $ \(_, v) -> 
      either (fmap (const []) . mapM_ err) (return . ok) v
   err e = $(logTM) ErrorS (logStr ("\tError:\t" ++ e))
   ok xs = map (^._3) $ sortOn (^._2) xs