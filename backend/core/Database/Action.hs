module Database.Action (Database.Action.runTryDbConn) where

import           KatipController
import           Database.Groundhog.Core
import           GHC.Exception.Type
import           Database.Exception
import           Data.Pool
import           Database.Groundhog.Postgresql

runTryDbConn :: TryAction Groundhog KatipController Postgresql a -> Pool Postgresql -> KatipController (Either SomeException a)
runTryDbConn = Database.Groundhog.Core.runTryDbConn