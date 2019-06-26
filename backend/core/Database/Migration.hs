module Database.Migration (run) where

import           Database.DbMeta  
import           Model.User.Entity (User)
import           Model.Token.Entity (Token)
import           Model.Rbac.Entity (Role, RoleTree)

import           Database.Groundhog.Postgresql
import           Data.Pool
import           Control.Monad.IO.Class
import           Database.Groundhog.Core (Action, PersistValue (..), fromPersistValues)
import           Database.Groundhog.Generic (firstRow)
import           Data.Word (Word32)
import           Data.Foldable (traverse_)
import           Data.Bool (bool)
import           Data.Time.Clock
import           Data.Typeable

run :: Pool Postgresql -> IO ()
run cm = (checkDBMeta >>= traverse_ (bool migrateInit migrateNew)) `runDbConn` cm

migrateInit :: Action Postgresql ()
migrateInit = populate >> setVersion

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
         \and table_name = '" <> 
         show (typeOf (undefined :: DbMeta)) <> "')" 
    stream <- queryRaw False sql []
    row <- firstRow stream
    traverse ((fst `fmap`) . fromPersistValues) row

getVersion :: Action Postgresql (Maybe Word32)
getVersion = fmap dbMetaMigrationVersion `fmap` get (DbMetaKey (PersistInt64 1)) 
    
setVersion :: Action Postgresql ()
setVersion = liftIO getCurrentTime >>= (insert_ . DbMeta 1)

populate :: Action Postgresql ()
populate = runMigration $
  do
    liftIO $ print "init migration start.."
    -- db meta 
    migrate (undefined :: DbMeta)
    migrate (undefined :: User)
    migrate (undefined :: Token)
    migrate (undefined :: Role)
    migrate (undefined :: RoleTree)