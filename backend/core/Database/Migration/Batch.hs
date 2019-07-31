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
import Control.Lens
import qualified Data.Map.Strict as Map
import Data.String.Interpolate

newtype Version = Version Word32
  deriving newtype Num
  deriving newtype Eq
  deriving newtype Ord
  deriving stock Show 

exec :: Version -> TryAction Groundhog (KatipContextT AppMonad) Postgresql (Maybe Version)
exec ver | list^?_last._1.to (+ 1) == Just ver = return Nothing 
exec _ | null list = return Nothing   
exec ver = maybe err ok (Map.fromList list Map.!? ver) 
  where
    ok (_, []) = throwError (MigrationSqlEmpty (coerce ver)) 
    ok (next, sql) = 
      do $(logTM) InfoS (logStr ([i|new migration from #{ver} to #{ver + 1}|] :: String))
         $(logTM) InfoS (logStr ([i|query: #{sql}|] :: String))
         executeRaw False sql []
         maybe (return (Just ver)) exec next
    err = throwError (MigrationNotFound (coerce ver))     
   
list :: [(Version, (Maybe Version, String))]
list = [(Version 2, (Nothing, V2.sql))]