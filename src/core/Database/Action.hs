{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Action 
      ( EdgeNodeAction
      , EdgeNodeActionKatip
      , runTryDbConnGH
      , runTryDbConnHasql
      , runTryDbConnHasqlR 
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

type EdgeNodeAction e = TryAction (Exception.Groundhog e) KatipController Postgresql

type EdgeNodeActionKatip e a = EdgeNodeAction e a ->  Pool Postgresql -> KatipController (Either SomeException a)

runTryDbConnGH 
  :: (KatipContext m, 
      MonadBaseControl IO m, 
      MonadCatch m, Typeable e, Show e) 
  => TryAction (Exception.Groundhog e) m Postgresql a 
  -> Pool Postgresql 
  -> m (Either SomeException a)
runTryDbConnGH action = 
    katipAddNamespace (Namespace ["db", "groundhog"]) 
  . runTryDbConn action

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

runTryDbConnHasqlR 
  :: Show a 
  => ReaderT KatipLoggerIO Session a
  -> Hasql.Pool
  -> KatipController (Either Exception.Hasql a)
runTryDbConnHasqlR action pool = 
  do 
    logger <- katipAddNamespace (Namespace ["db", "hasql"]) askLoggerIO
    dbRes <- liftIO $ Hasql.use pool (runReaderT action logger)
    for_ dbRes (liftIO . logger InfoS . logStr . show)
    return $ first hasqlToException dbRes

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
