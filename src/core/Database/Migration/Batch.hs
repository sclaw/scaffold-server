{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Migration.Batch (Version (..), exec) where 

import EdgeNode.Application

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

import Debug.Trace 

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

$mkMigrationSeq

exec :: Version -> TryAction (Groundhog ()) (KatipContextT AppMonad) Postgresql (Maybe Version) 
exec _ | null list = return Nothing    
exec ver = maybe err ok (migrMap Map.!? ver) 
  where  
    ok val = 
      case val of
        Stop -> return $ Just ver
        NextSql sql ver -> withSql sql >> exec ver
        NextMigration migr ver -> 
          mkSql migr >>= mapM_ withSql >> exec ver
    withSql sql | null sql = $(logTM) InfoS "migration empty"      
    withSql sql = 
      do 
        $(logTM) InfoS (logStr ([i|new migration from #{ver} to #{ver + 1}|] :: String))
        $(logTM) InfoS (logStr ([i|query: #{sql}|] :: String))
        executeRaw False sql []     
    err = throwError (MigrationNotFound (coerce ver))     
    migrMap = Map.fromList list

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