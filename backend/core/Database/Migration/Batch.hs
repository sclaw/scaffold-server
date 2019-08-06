{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Migration.Batch (Version (..), exec) where 

import EdgeNode.Application
import qualified Database.Migration.V2 as V2
import qualified Database.Migration.V3 as V3

import Data.Word (Word32)
import Database.Exception
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Katip
import Control.Monad.Except
import Data.Coerce
import qualified Data.Map.Strict as Map
import Data.String.Interpolate

newtype Version = Version Word32
  deriving newtype Num
  deriving newtype Eq
  deriving newtype Ord
  deriving stock Show 

data MigrationStep = Next String Version | Stop deriving Eq

exec :: Version -> TryAction Groundhog (KatipContextT AppMonad) Postgresql (Maybe Version) 
exec _ | null list = return Nothing    
exec ver = maybe err ok (migrMap Map.!? ver) 
  where
    ok tpl | isEmpty tpl = throwError (MigrationSqlEmpty (coerce ver))  
    ok val = 
      case val of
        Stop -> return (Just ver)
        Next sql ver -> withSql sql >> exec ver
    withSql sql = 
      do 
        $(logTM) InfoS (logStr ([i|new migration from #{ver} to #{ver + 1}|] :: String))
        $(logTM) InfoS (logStr ([i|query: #{sql}|] :: String))
        executeRaw False sql []     
    err = throwError (MigrationNotFound (coerce ver))     
    migrMap = Map.fromList list

isEmpty :: MigrationStep -> Bool
isEmpty (Next sql _) = null sql
isEmpty Stop = False

list :: [(Version, MigrationStep)]
list = 
  [ (Version 1, Next V2.sql (Version 2))
  , (Version 2, Next V3.sql (Version 3))
  , (Version 3, Stop)
  ]