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
import Data.Maybe
import Control.Monad
import Control.Applicative

type MigrationAction a = TryAction Exception.Groundhog (KatipContextT App.AppMonad) Postgresql a

run :: Pool Postgresql -> KatipContextT App.AppMonad (Either SomeException ())
run cm = (checkDBMeta >>= traverse_ (bool Database.Migration.init (Database.Migration.migrate Nothing))) `runTryDbConnGH` cm

init :: MigrationAction ()
init = 
  do 
    Database.Table.print 
    mkTables
    Database.Migration.migrate (Just 1)

migrate :: Maybe Word32 -> MigrationAction ()
migrate init = 
  do
    v <- (<|> init) `fmap` getVersion
    let batch = Batch.Version `fmap`init
    for_ (v <|> init) $ \i -> do
      $(logTM) InfoS (logStr ("migration will be start from version " <> (i + 1)^.stringify))     
      next <- Batch.exec (Batch.Version (i + 1))
      for_ (next <|> batch) $ \ident -> do
        tm <- liftIO getCurrentTime
        let v = ident^.coerced 
        setVersion tm (maybe (New v) (const (Init v)) init)
        $(logTM) InfoS (logStr ("migration finished at version " <> show ident))
      when (isNothing next) $ $(logTM) InfoS (logStr ("no migration found" :: String))  

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

data SetVrsion = Init Word32 | New Word32   

setVersion :: UTCTime -> SetVrsion -> MigrationAction ()
setVersion tm (Init v) = insert_ (DbMeta v tm)
setVersion tm (New v) = replace (DbMetaKey (PersistInt64 1)) (DbMeta v tm)