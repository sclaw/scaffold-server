module Database.Migration (run) where

import           Model.User.Entity (User)
import           Model.Token.Entity (Token)
import           Model.Rbac.Entity (Role, RoleTree)

import           Database.Groundhog.Postgresql
import           Data.Pool
import           Control.Monad.IO.Class
import           Database.Groundhog.Core 
                 (NamedMigrations
                 , Action
                 , PersistValue(..)
                 , fromPersistValues)
import           Data.Word (Word32)
import           Database.Groundhog.Generic (firstRow)
import           Data.Foldable (traverse_)
import           Data.Bool (bool)
import           Control.Monad.State.Lazy (modify')
import qualified Data.Map.Strict as Map

run :: Pool Postgresql -> IO ()
run cm = (checkDBMeta >>= traverse_ (bool migrateInit migrateNew)) `runDbConn` cm

migrateInit :: Action Postgresql ()
migrateInit =
  do 
    buildTables
    setVersion

migrateNew :: Action Postgresql ()
migrateNew = 
  do
    v <- getVersion
    let skip = liftIO $ print "new migration not found. skip"
    traverse_ (const skip) v

checkDBMeta :: Action Postgresql (Maybe Bool)
checkDBMeta = 
  do 
    let sql = 
         "select exists (select 1 \
         \from information_schema.tables \ 
         \where table_schema = 'public' \
         \and table_name = 'db_meta')" 
    stream <- queryRaw False sql []
    row <- firstRow stream
    traverse ((fst `fmap`) . fromPersistValues) row

getVersion :: Action Postgresql (Maybe Word32)
getVersion =
    do
    stream <- queryRaw False "select \"migrationVersion\" from db_meta" []
    row <- firstRow stream
    traverse ((fst `fmap`) . fromPersistValues) row

setVersion :: Action Postgresql ()
setVersion = executeRaw False sql [PersistInt64 1]
  where 
    sql = "insert into db_meta (\"migrationVersion\", \
          \\"modificationTime\") values (?, now())"

buildTables :: Action Postgresql ()
buildTables = runMigration $ do
    -- db meta 
    modify' mkDBMetaMigration
    migrate (undefined :: User)
    migrate (undefined :: Token)
    migrate (undefined :: Role)
    migrate (undefined :: RoleTree)

mkDBMetaMigration :: NamedMigrations -> NamedMigrations
mkDBMetaMigration = 
  Map.insert 
  "db_meta"  
  (Right [(False, 1, mkDBMetaTable)])
  where
    mkDBMetaTable = 
      "create table if not exists db_meta \
      \(\"migrationVersion\" integer not null default 0, \
      \\"modificationTime\" timestamp)"