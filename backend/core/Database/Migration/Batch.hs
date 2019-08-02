{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Migration.Batch (Version (..), exec) where 

import EdgeNode.Application
import qualified Database.Migration.V2 as V2

import Data.Word (Word32)
import Database.Exception
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Katip
import Control.Monad.Except
import Data.Coerce
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Control.Lens

newtype Version = Version Word32
  deriving newtype Num
  deriving newtype Eq
  deriving newtype Ord
  deriving stock Show 

data Migration = Next String | Single String 

exec :: Version -> TryAction Groundhog (KatipContextT AppMonad) Postgresql (Maybe Version) 
exec _ | null list = return Nothing
exec ver | list^?_last._1.to (+ 1) == Just ver = return Nothing    
exec ver = maybe err ok (Map.fromList list Map.!? ver) 
  where
    ok sql | isEmpty sql = throwError (MigrationSqlEmpty (coerce ver)) 
    ok sql = 
      do $(logTM) InfoS (logStr ([i|new migration from #{ver} to #{ver + 1}|] :: String))
         $(logTM) InfoS (logStr ([i|query: #{getSql sql}|] :: String))
         executeRaw False (getSql sql) []
         case sql of 
          Next _ -> exec (ver + 1)
          Single _ -> return $ Just ver
    err = throwError (MigrationNotFound (coerce ver))     
   
getSql :: Database.Migration.Batch.Migration -> String 
getSql (Next sql) = sql
getSql (Single sql) = sql

isEmpty :: Database.Migration.Batch.Migration -> Bool
isEmpty (Next sql) = null sql
isEmpty (Single sql) = null sql

list :: [(Version, Database.Migration.Batch.Migration)]
list = [(Version 2, Single V2.sql)]