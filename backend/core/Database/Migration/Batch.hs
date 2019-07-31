{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Migration.Batch (Version (..), exec) where 

import EdgeNode.Application

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
exec ver = maybe err ok (map Map.!? ver) 
  where
    ok (next, sql) = 
      do $(logTM) InfoS (logStr ([i|new migration from #{ver} to #{ver + 1}|] :: String))
         $(logTM) InfoS (logStr ([i|query #{sql}|] :: String))
         void $ queryRaw False sql []
         maybe (return (Just ver)) exec next
    err = throwError (MigrationNotFound (coerce ver))     
    map = Map.fromList list      

list :: [(Version, (Maybe Version, String))]
list = []