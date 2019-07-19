{-# LANGUAGE OverloadedStrings #-}

module Database.Action (runTryDbConnGH, runTryDbConnHasql) where

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

runTryDbConnGH 
  :: TryAction Exception.Groundhog KatipController Postgresql a 
  -> Pool Postgresql 
  -> KatipController (Either SomeException a)
runTryDbConnGH action = katipAddNamespace (Namespace ["orm"]) . runTryDbConn action

runTryDbConnHasql :: Show a => (KatipLoggerIO -> Session a) -> Hasql.Pool -> KatipController (Either Exception.Hasql a)
runTryDbConnHasql action pool = 
  do 
    logger <- katipAddNamespace (Namespace ["raw"]) askLoggerIO
    dbRes <- liftIO $ Hasql.use pool (action logger)
    for_ dbRes (liftIO . logger InfoS . logStr . show)
    return $ first fromHasqlToServerExcep dbRes

fromHasqlToServerExcep :: Hasql.UsageError -> Exception.Hasql
fromHasqlToServerExcep 
  e@(Hasql.SessionError 
   (Hasql.QueryError _ _ 
    (Hasql.ResultError 
     (Hasql.ServerError code msg _ _)))) 
  | foreign_key_violation == code = 
    Exception.ForeignKeyViolation msg
  | unique_violation == code = 
    Exception.UniqueViolation msg
  | otherwise = Exception.OtherError e 
fromHasqlToServerExcep e = Exception.OtherError e