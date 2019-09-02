{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Action 
      ( EdgeNodeAction
      , EdgeNodeActionKatip
      , runTryDbConnGH
      , runTryDbConnHasql
      , render
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
import Database.Groundhog.Generic.Sql
import Control.Lens.Iso.Extended
import Control.Lens

type EdgeNodeAction e = TryAction (Exception.Groundhog e) KatipController Postgresql

type EdgeNodeActionKatip e a = EdgeNodeAction e a ->  Pool Postgresql -> KatipController (Either SomeException a)

runTryDbConnGH 
  :: (KatipContext m, 
      MonadBaseControl IO m, 
      MonadCatch m, Typeable e, Show e) 
  => TryAction (Exception.Groundhog e) m Postgresql a 
  -> Pool Postgresql 
  -> m (Either SomeException a)
runTryDbConnGH action = katipAddNamespace (Namespace ["orm"]) . runTryDbConn action

runTryDbConnHasql 
  :: Show a 
  => (KatipLoggerIO -> Session a) 
  -> Hasql.Pool
  -> KatipController (Either Exception.Hasql a)
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

render :: Cond Postgresql r -> [PersistValue] -> String
render cond xs = fromUtf8 (maybe mempty mkString (renderCond (RenderConfig id) cond))^.from textbs.from stext
  where mkString r = getQuery r <> intercalateS "," (map (fromString . defaultShowPrim) (getValues r xs)) 

