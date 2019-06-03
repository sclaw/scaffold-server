module Database.Action (runTryDbConnOrm, runTryDbConnRaw) where

import           KatipController
import           Database.Groundhog.Core
import           GHC.Exception.Type
import           Database.Exception
import           Data.Pool
import           Database.Groundhog.Postgresql
import           Katip
import qualified Hasql.Pool as Hasql 
import           Hasql.Session (Session) 
import           Control.Monad.IO.Class

runTryDbConnOrm :: TryAction Groundhog KatipController Postgresql a -> Pool Postgresql -> KatipController (Either SomeException a)
runTryDbConnOrm action = katipAddNamespace (Namespace ["orm"]) . runTryDbConn action

runTryDbConnRaw :: Session a -> Hasql.Pool -> KatipController (Either Hasql.UsageError a)
runTryDbConnRaw action = katipAddNamespace (Namespace ["raw"]) . liftIO . flip Hasql.use action