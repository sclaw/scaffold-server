{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.Transaction 
      ( transaction
      , transactionHasql
      , katipTransaction
      , statement
      , SessionR
      , ParamsShow (..)
      , ask
      , lift
      ) where

import KatipController
import GHC.Exception.Type
import Data.Pool
import Katip
import qualified Hasql.Session as Hasql
import Hasql.Session (Session) 
import Control.Monad.IO.Class
import Katip.Monadic (askLoggerIO)
import Control.Monad.Catch
import Control.Monad.Reader
import qualified Hasql.Connection as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.Transaction as HasqlT
import qualified Hasql.Transaction.Sessions as HasqlT
import  Control.Exception (throwIO)
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Int
import Data.Coerce
import Data.List

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
transaction pool logger session = 
  fmap join run >>= 
  either (throwIO . QueryErrorWrapper) pure
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

transactionHasql :: Pool Hasql.Connection -> KatipLoggerIO -> ReaderT KatipLoggerIO HasqlT.Transaction a -> IO a
transactionHasql pool logger session =
  withResource pool $ \conn -> do
    let runT = HasqlT.transaction HasqlT.ReadCommitted HasqlT.Write 
    resp <- Hasql.run (runReaderT (mapReaderT runT session) logger) conn  
     `onException`  Hasql.run (runT HasqlT.condemn) conn
    either (throwIO . QueryErrorWrapper) pure resp

class ParamsShow a where
  render :: a -> String

instance ParamsShow () where render () = mempty
instance ParamsShow Int32 where render = show
instance ParamsShow Int64 where render = show
instance (ParamsShow a, ParamsShow b) => ParamsShow (a, b) where 
  render (x, y) = render x <> ", " <> render y
instance (ParamsShow a, ParamsShow b, ParamsShow c) => ParamsShow (a, b, c) where 
  render x = render (x^._1) <> ", " <> render (x^._2) <> ", " <> render (x^._3)
instance (ParamsShow a, ParamsShow b, ParamsShow c, ParamsShow d) => ParamsShow (a, b, c, d) where 
    render x = render (x^._1) <> ", " <> render (x^._2) <> ", " <> render (x^._3) <> ", " <> render (x^._4) 
instance ParamsShow a => ParamsShow [a] where 
  render xs = intercalate ", " $ map render xs
instance {-# OVERLAPS #-} (Coercible a b, Show b) => ParamsShow a where render = show . coerce @a @b

statement :: ParamsShow a => Hasql.Statement a b -> a -> ReaderT KatipLoggerIO Session b
statement s@(Hasql.Statement sql _ _ _) a = do 
  logger <- ask
  liftIO $ logger DebugS (ls (sql <> " [" <> (render a^.stext.textbs)) <> "]") 
  lift $ Hasql.statement a s

katipTransaction :: Pool Hasql.Connection -> ReaderT KatipLoggerIO Session a -> KatipController a
katipTransaction pool session = katipAddNamespace (Namespace ["db", "hasql"]) askLoggerIO >>= (liftIO . flip (transaction pool) session)