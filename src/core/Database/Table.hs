{-# LANGUAGE TemplateHaskell #-}

module Database.Table (mkTables, Database.Table.print) where

import qualified EdgeNode.Application as App
import EdgeNode.Model.User (User)

import Database.DbMeta  
import Database.Groundhog.Core
import Database.Groundhog.Postgresql
import qualified Database.Exception as Exception
import Katip
import qualified Data.Map as Map
import Control.Monad

mkTables :: TryAction (Exception.Groundhog ()) (KatipContextT App.AppMonad) Postgresql ()
mkTables = runMigration migration
   
print :: TryAction (Exception.Groundhog ()) (KatipContextT App.AppMonad) Postgresql ()
print = createMigration migration >>= print
  where 
   print migs = 
    forM_ (Map.assocs migs) $ \(k, v) -> do
      $(logTM) InfoS (logStr ("Datatype " ++ k ++ ":"))
      case v of
       Left errors -> mapM_ (\e -> $(logTM) InfoS (logStr ("\tError:\t" ++ e))) errors
       Right sqls  -> do
        let showSql (isUnsafe, _, sql) = (if isUnsafe then "Unsafe:\t" else "Safe:\t") ++ sql
        mapM_ ($(logTM) InfoS . logStr . ("\t" ++) . showSql) sqls

migration :: Migration (TryAction (Exception.Groundhog ()) (KatipContextT App.AppMonad) Postgresql)
migration = 
  do 
    migrate (undefined :: DbMeta)
    migrate (undefined :: User)