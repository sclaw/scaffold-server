module Database.Table (mkTables) where

import EdgeNode.Model.User.Entity (User)
import EdgeNode.Model.Token.Entity (Token)
import EdgeNode.Model.Rbac.Entity (RoleTree)

import Database.DbMeta  
import Database.Groundhog.Core (Action)
import Database.Groundhog.Postgresql
import Control.Monad.IO.Class

mkTables :: Action Postgresql ()
mkTables = runMigration $
  do
    liftIO $ print "init migration start.."
    -- db meta 
    migrate (undefined :: DbMeta)
    migrate (undefined :: User)
    migrate (undefined :: Token)
    migrate (undefined :: RoleTree)