{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Statement.Rbac 
       ( getTopLevelRoles
       , isPermissionBelongToRole
       , assignRoleToUser
       ) where

import EdgeNode.Model.Rbac
import EdgeNode.Transport.Id

import qualified Hasql.Statement as HS
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Foldl
import Hasql.TH
import Data.Int

getTopLevelRoles :: HS.Statement (Id, Permission) [Id]
getTopLevelRoles = lmap (bimap (^.coerced) (^.isoPermission.stext)) $ statement $ premap (^.coerced) list                
  where
    statement = 
      [foldStatement| 
        with recursive 
          roles as (
           select ur.role_fk, array[ur.role_fk] as arr 
           from auth.user_role as ur 
           where ur.user_fk = $1 :: int8
           union all
             select r.id, array_append(rs.arr, r.id)
             from auth.role as r
             join roles as rs
             on r.parent_fk = rs.role_fk)
        select r.arr[1] :: int8 
        from roles as r
        inner join (select role_fk 
                   from auth.role_permission 
                   where permission_fk in 
                   (select id from auth.permission 
                    where title = $2 :: text)) as rp
        on r.role_fk = rp.role_fk|]

isPermissionBelongToRole :: HS.Statement ([Id], Permission) Bool
isPermissionBelongToRole = lmap (bimap (^..traversed.coerced) (^.isoPermission.stext)) statement
  where 
    statement = 
      [singletonStatement|
        with recursive 
          perms as (
           select id, title
           from auth.permission
           where id in 
             (select permission_fk 
              from auth.role_permission 
              where role_fk = any($1 :: int8[]))
           union
             select p.id, p.title 
             from perms as ps
             inner join auth.permission as p
             on ps.id = p.parent_fk)
        select ((count(*) > 0) :: bool) from perms where title = $2 :: text|]

assignRoleToUser :: HS.Statement (Int64, Role, Int64) ()
assignRoleToUser = lmap (& _2 %~ (^.isoRole.stext)) $
  [resultlessStatement|
    insert into auth.user_role 
    (user_fk, role_fk, admin_fk) 
    select $1 :: int8, id, $3 :: int8 
    from auth.role where title = $2 :: text|]