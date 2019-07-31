module Database.Migration.Batch (Version (..), exec) where 

import EdgeNode.Application

import Data.Word (Word32)
import Database.Exception
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import Katip
import Control.Monad.Except
import Data.Coerce

newtype Version = Version Word32

exec :: Version -> TryAction Groundhog (KatipContextT AppMonad) Postgresql Version
exec i = throwError $ MigrationNotFound (coerce i) 