module Database.Action (Database.Action.runTryDbConn) where

import           KatipHandler
import           Database.Groundhog.Core
import           GHC.Exception.Type
import           Database.Exception
import           Data.Pool
import           Database.Groundhog.Postgresql

runTryDbConn :: TryAction Groundhog KatipHandler Postgresql a -> Pool Postgresql -> KatipHandler (Either SomeException a)
runTryDbConn = Database.Groundhog.Core.runTryDbConn