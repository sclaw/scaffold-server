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

import KatipController
import Data.Word (Word32)
import Katip
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import TH.Mk
import qualified Data.ByteString as B
import qualified Hasql.Session
import Control.Monad.IO.Class

newtype Version = Version Word32
  deriving newtype Num
  deriving newtype Eq
  deriving newtype Ord
  deriving stock Show 

data MigrationStep = NextSql B.ByteString Version | Stop deriving Show

$mkMigrationSeq

exec :: Version -> KatipLoggerIO -> Hasql.Session.Session (Maybe Version) 
exec _ _ | null list = return Nothing    
exec ver logger = maybe (error ("migration not found: " <> show ver)) ok ((Map.fromList list) Map.!? ver) 
  where  
    ok Stop = return $ Just ver
    ok (NextSql sql ver) = do
      liftIO $ logger InfoS (logStr ([i|migration from #{ver} to #{ver + 1}|] :: String))
      liftIO $ logger InfoS (logStr ([i|query: #{sql}|] :: String))
      Hasql.Session.sql sql
      exec ver logger