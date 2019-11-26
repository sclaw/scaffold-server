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
           select ur.rolefk, array[ur.rolefk] as arr 
           from "edgeNode"."UserRole" as ur 
           where ur.userfk = $1
           union all
             select r.id, array_append(rs.arr, r.id)
             from "edgeNode"."Role" as r
             join roles as rs
             on r.parent = rs.rolefk)
        select rs1.arr[1] 
        from roles as rs1
        inner join (select rolefk 
                   from "edgeNode"."RolePermission" 
                   where permissionfk in 
                   (select id from "edgeNode"."Permission" 
                    where title = $2)) as rs2
        on rs1.rolefk = rs2.rolefk
      |]
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
           from "edgeNode"."Permission"
           where id in 
             (select permissionfk 
              from "edgeNode"."RolePermission" 
              where rolefk = any($1))
           union
             select p.id, p.title 
             from "edgeNode"."Permission" as p
             join perms as ps
             on ps.id = p.parent)
        select count(*) > 0 from perms where title = $2     
      |]
    encoder = 
      contramap 
      (^.._1.traversed._Wrapped') 
      (HE.param (HE.nonNullable 
       (HE.array (HE.dimension foldl' 
        (HE.element (HE.nonNullable HE.int8)))))) <>
      contramap (^._2) (HE.param (HE.nonNullable (HE.enum (^.isoPermission.stext))))
    decoder = HD.rowMaybe $ HD.column (HD.nonNullable HD.bool)