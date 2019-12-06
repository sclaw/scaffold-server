{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Action 
      ( runTryDbConnHasql
      , transaction
      , katipTransaction
      , SessionR
      , ask
      , lift
      ) where

import KatipController
import Database.Groundhog.Core
import GHC.Exception.Type
import qualified Database.Exception as Exception
import Data.Pool
import Database.Groundhog.Postgresql
import Katip
import qualified Hasql.Pool as Hasql
import qualified Hasql.Session as Hasql
import Hasql.Session (Session) 
import Control.Monad.IO.Class
import Data.Bifunctor
import Katip.Monadic (askLoggerIO)
import PostgreSQL.ErrorCodes
import Data.Foldable 
import Control.Monad.Trans.Control
import Control.Monad.Catch
import Data.Typeable
import Control.Monad.Reader
import qualified Hasql.Connection as Hasql
import  Control.Exception (throwIO)

runTryDbConnHasql 
  :: Show a 
  => (KatipLoggerIO -> Session a) 
  -> Hasql.Pool
  -> KatipController (Either Exception.Hasql a)
runTryDbConnHasql action pool = 
  do 
    logger <- katipAddNamespace (Namespace ["db", "hasql"]) askLoggerIO
    dbRes <- liftIO $ Hasql.use pool (action logger)
    for_ dbRes (liftIO . logger InfoS . logStr . show)
    return $ first hasqlToException dbRes

newtype QueryErrorWrapper = QueryErrorWrapper Hasql.QueryError 
  deriving Show

instance Exception QueryErrorWrapper


type SessionR a = ReaderT KatipLoggerIO Session a

-- | Run exception-safe database transaction. User action is run inside
-- transaction.
--
-- If user method returns a value then transaction is properly commited,
--
-- If user methods throws an exception then transaction is rolledback,
-- connection is destroyed and exception is rethrown.
--
-- Summary: if this method returns a value, then transaction is commited
-- and connection is returned back to the pool.
--
-- Additional notes:
--
-- Transaction handle takes a Logger as an argument, this logger
-- will be used in all queries inside transaction.
--
-- 'TransactionHandle' is valid only inside transaction scope,
-- using it outside transaction will lead to undefined behavior.
--
-- This method may throw 'SQLException' in case if SQL exception
-- happens during operations that commit or rollback transaction,
-- in this case connection may not be in a clean state.
transaction :: Pool Hasql.Connection -> KatipLoggerIO -> ReaderT KatipLoggerIO Session a -> IO a
transaction pool logger session = fmap join run >>= either (throwIO . QueryErrorWrapper) pure
  where
    run = withResource pool $ \conn ->
      mask $ \release -> do 
        begin <- Hasql.run (Hasql.sql "begin") conn
        let withBegin = do
              result <- 
                release (Hasql.run (runReaderT session logger) conn >>= 
                  either (throwIO . QueryErrorWrapper) pure)
                 `onException` 
                Hasql.run (Hasql.sql "rollback") conn
              commit <- Hasql.run (Hasql.sql "commit") conn    
              traverse (const (pure result)) commit
        traverse (const withBegin) begin

katipTransaction :: Pool Hasql.Connection -> ReaderT KatipLoggerIO Session a -> KatipController a
katipTransaction pool session = do 
  logger <- katipAddNamespace (Namespace ["db", "hasql"]) askLoggerIO
  liftIO $ transaction pool logger session

hasqlToException :: Hasql.UsageError -> Exception.Hasql
hasqlToException 
  e@(Hasql.SessionError 
   (Hasql.QueryError _ _ 
    (Hasql.ResultError 
     (Hasql.ServerError code msg _ _)))) 
  | foreign_key_violation == code = 
    Exception.ForeignKeyViolation msg
  | unique_violation == code = 
    Exception.UniqueViolation msg
  | otherwise = Exception.OtherError e 
hasqlToException e = Exception.OtherError e