{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Migration (run) where

import qualified EdgeNode.Application as App

import Database.Action (runTryDbConnGH)  
import Database.DbMeta  
import Database.Groundhog.Postgresql
import Data.Pool
import Control.Monad.IO.Class
import Database.Groundhog.Core
import Database.Groundhog.Generic (firstRow)
import Data.Word (Word32)
import Data.Bool (bool)
import Data.Time.Clock
import Data.Typeable
import Database.Table
import qualified Database.Exception as Exception
import Control.Exception.Base
import Katip
import Control.Lens
import Control.Lens.Iso.Extended
import Data.String.Interpolate
import Data.Foldable
import qualified Database.Migration.Batch as Batch 

type MigrationAction a = TryAction Exception.Groundhog (KatipContextT App.AppMonad) Postgresql a

run :: Pool Postgresql -> KatipContextT App.AppMonad (Either SomeException ())
run cm = (checkDBMeta >>= traverse_ (bool migrateInit migrateNew)) `runTryDbConnGH` cm

migrateInit :: MigrationAction ()
migrateInit = 
  do 
    Database.Table.print 
    mkTables
    setVersion Nothing

migrateNew :: MigrationAction ()
migrateNew = 
  do
    v <- getVersion
    $(logTM) InfoS (logStr ("migration last version " <> v^.stringify))
    for_ v $ \i -> do
      $(logTM) InfoS (logStr ("migration will be start from version " <> show (i + 1)))     
      next <- Batch.exec (Batch.Version (i + 1))
      for_ next $ \ident -> setVersion (Just (ident^.coerced))

checkDBMeta :: MigrationAction (Maybe Bool)
checkDBMeta = 
  do 
    let tbl = show (typeOf (undefined :: DbMeta)) 
    let sql = 
         [i| select exists (select 1
           from information_schema.tables 
           where table_schema = 'public'
           and table_name = '#{tbl}')
         |]   
    stream <- queryRaw False sql []
    row <- firstRow stream
    traverse ((fst `fmap`) . fromPersistValues) row

getVersion :: MigrationAction (Maybe Word32)
getVersion =
  do 
    let sql =
          [i|select "dbMetaMigrationVersion"  from "DbMeta" 
            order by "dbMetaMigrationVersion" desc limit 1  
          |]
    stream <- queryRaw False sql []
    row <- firstRow stream
    traverse (return . fromPrimitivePersistValue . head) row
         
setVersion :: Maybe Word32 -> MigrationAction ()
setVersion v = liftIO getCurrentTime >>= \tm -> maybe (insert_ (DbMeta 1 tm)) (replace (DbMetaKey (PersistInt64 1)) . flip DbMeta tm) v