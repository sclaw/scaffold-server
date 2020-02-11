{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Transaction 
      ( transactionE
      , katipTransactionE
      , transaction
      , katipTransaction
      , statement
      , SessionR
      , ParamsShow (..)
      , ask
      , lift
      ) where

import EdgeNode.Transport.Error

import KatipController
import GHC.Exception.Type
import Data.Pool
import Katip
import qualified Hasql.Session as Hasql
import Hasql.Session (Session, QueryError (..), CommandError (..), ResultError (..)) 
import Control.Monad.IO.Class
import Katip.Monadic (askLoggerIO)
import Control.Monad.Catch
import Control.Monad.Reader
import qualified Hasql.Connection as Hasql
import qualified Hasql.Statement as Hasql
import Control.Exception (throwIO)
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Int
import Data.Coerce
import Data.List
import qualified Data.ByteString.Char8 as B
import Data.Generics.Sum.Constructors
import Data.Generics.Product.Positions
import GHC.Generics
import PostgreSQL.ErrorCodes
import qualified Data.Text as T

newtype QueryErrorWrapper = QueryErrorWrapper Hasql.QueryError 
  deriving Show

instance Exception QueryErrorWrapper

type SessionR a = ReaderT KatipLoggerIO Session a

deriving instance Generic Hasql.QueryError
deriving instance Generic CommandError
deriving instance Generic ResultError

data ViolationError = ForeignKeyVlolation | UniqueViolation
  deriving Show

instance AsError ViolationError where
  asError ForeignKeyVlolation = asError @T.Text "foreign key violation"
  asError UniqueViolation = asError @T.Text "unique key violation"

instance Exception ViolationError

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
transactionE :: Pool Hasql.Connection -> KatipLoggerIO -> ReaderT KatipLoggerIO Session a -> IO (Either ViolationError a)
transactionE pool logger session = 
  fmap join run >>= 
  either (throwIO . QueryErrorWrapper) pure
  where
    run = withResource pool $ \conn ->
      mask $ \release -> do 
        bg <- begin conn
        let action =
              Hasql.run (runReaderT session logger) conn >>= 
                handleDBResult
        let withBegin = do
              result <- release action `onException` rollback conn
              cm <- commit conn    
              traverse (const (pure result)) cm
        traverse (const withBegin) bg

handleDBResult :: Either Hasql.QueryError a -> IO (Either ViolationError a)
handleDBResult (Left e) =
  case codem of 
    Just code ->
      if code == foreign_key_violation 
      then return $ Left ForeignKeyVlolation
      else if code == unique_violation 
      then return $ Left UniqueViolation 
      else throwIO $ QueryErrorWrapper e
    Nothing -> throwIO $ QueryErrorWrapper e
  where codem = e^?position @3._Ctor @"ResultError"._Ctor @"ServerError"._1     
handleDBResult (Right  val) = return $ Right val

begin, commit, rollback :: Hasql.Connection -> IO (Either Hasql.QueryError ())
begin = Hasql.run (Hasql.sql "begin")
commit = Hasql.run (Hasql.sql "commit")
rollback = Hasql.run (Hasql.sql "rollback")

transaction :: Pool Hasql.Connection -> KatipLoggerIO -> ReaderT KatipLoggerIO Session a -> IO a
transaction pool logger session = transactionE pool logger session >>= either throwIO  pure

class ParamsShow a where
  render :: a -> String

instance ParamsShow () where render () = mempty
instance ParamsShow Int32 where render = show
instance ParamsShow Int64 where render = show
instance ParamsShow B.ByteString where render = B.unpack
instance ParamsShow a => ParamsShow (Maybe a) where render = maybe mempty render 
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
  liftIO $ logger DebugS (ls (sql <> " { params: [" <> (render a^.stext.textbs)) <> "] }") 
  lift $ Hasql.statement a s

katipTransaction :: Pool Hasql.Connection -> ReaderT KatipLoggerIO Session a -> KatipController a
katipTransaction pool session = katipAddNamespace (Namespace ["db", "hasql"]) askLoggerIO >>= (liftIO . flip (transaction pool) session)

katipTransactionE :: Pool Hasql.Connection -> ReaderT KatipLoggerIO Session a -> KatipController (Either ViolationError a)
katipTransactionE pool session = katipAddNamespace (Namespace ["db", "hasql"]) askLoggerIO >>= (liftIO . flip (transactionE pool) session)