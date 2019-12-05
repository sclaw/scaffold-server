{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Statement.Rbac (getTopLevelRoles, EdgeNode.Statement.Rbac.elem) where

import EdgeNode.Model.User
import EdgeNode.Model.Rbac

import qualified Hasql.Statement as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.String.Interpolate
import Control.Lens
import Control.Lens.Iso.Extended
import Data.Foldable

getTopLevelRoles :: HS.Statement (UserId, Permission) [RoleId]
getTopLevelRoles = HS.Statement sql encoder decoder False
  where
    sql = 
      [i|with recursive roles as (
           select ur.role_fk, array[ur.role_fk] as arr 
           from auth.user_role as ur 
           where ur.user_fk = $1
           union all
             select r.id, array_append(rs.arr, r.id)
             from auth.role as r
             join roles as rs
             on r.parent_fk = rs.role_fk)
        select rs1.arr[1] 
        from roles as rs1
        inner join (select role_fk 
                   from auth.role_permission 
                   where permission_fk in 
                   (select id from auth.permission 
                    where title = $2)) as rs2
        on rs1.role_fk = rs2.role_fk|]
    encoder = 
      contramap (^._1._Wrapped') (HE.param (HE.nonNullable HE.int8)) <>
      contramap (^._2) (HE.param (HE.nonNullable (HE.enum (^.isoPermission.stext))))
    decoder = HD.rowList $ HD.column (HD.nonNullable HD.int8) <&> (^._Unwrapped')

elem :: HS.Statement ([RoleId], Permission) (Maybe Bool)
elem = HS.Statement sql encoder decoder False
  where 
    sql = 
      [i|with recursive perms as (
           select id, title
           from auth.permission
           where id in 
             (select permission_fk 
              from auth.role_permission 
              where role_fk = any($1))
           union
             select p.id, p.title 
             from perms as ps
             inner join auth.permission as p
             on ps.id = p.parent_fk)
        select count(*) > 0 from perms where title = $2|]
    encoder = 
      contramap 
      (^.._1.traversed._Wrapped') 
      (HE.param (HE.nonNullable 
       (HE.array (HE.dimension foldl' 
        (HE.element (HE.nonNullable HE.int8)))))) <>
      contramap (^._2) (HE.param (HE.nonNullable (HE.enum (^.isoPermission.stext))))
    decoder = HD.rowMaybe $ HD.column (HD.nonNullable HD.bool)