{-# LANGUAGE OverloadedStrings #-}

module Database.Action (runTryDbConnGH, runTryDbConnHasql) where

import           KatipController
import           Database.Groundhog.Core
import           GHC.Exception.Type
import qualified Database.Exception as Exception
import           Data.Pool
import           Database.Groundhog.Postgresql
import           Katip
import qualified Hasql.Pool as Hasql 
import           Hasql.Session (Session) 
import           Control.Monad.IO.Class

runTryDbConnGH 
  :: TryAction Exception.Db KatipController Postgresql a 
  -> Pool Postgresql 
  -> KatipController (Either SomeException a)
runTryDbConnGH action = katipAddNamespace (Namespace ["orm"]) . runTryDbConn action

runTryDbConnHasql :: Session a -> Hasql.Pool -> KatipController (Either Hasql.UsageError a)
runTryDbConnHasql action = liftIO . flip Hasql.use action