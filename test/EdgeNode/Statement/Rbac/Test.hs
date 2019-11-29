{-# LANGUAGE QuasiQuotes #-}

module EdgeNode.Statement.Rbac.Test (spec_rbac) where

import qualified EdgeNode.Statement.Rbac as Rbac 
import EdgeNode.Model.User
import EdgeNode.Model.Rbac

import Database.Migration.Test
import Test.Hspec hiding (shouldBe)
import Test.Hspec.DB.Hasql
import Hasql.Session
import Test.Hspec.Expectations.Lifted
import Data.String.Interpolate
import Debug.Trace ()


----- test ----- 
{-

                             r1 (id = 1)
                                 |
               __________________|____________________
               |                                     |
            r2 (id = 2)                          r5 (id = 3)
               |                                     |
       ________|________                     ________|________
       |               |                    |                |
  r3 (id = 4)      r4 (id = 5)         r6 (id = 6)        r7 (id = 7)
                                            |
                                       r8 (id = 8)
                                       
                          root (id = 1)
                               |
                          providerAdmin (id = 2)
                               |
                          providerGuest (id = 3)

    r6 -> root 
    r5 -> root
    r6 -> providerAdmin

    user (id = 1) -> r5
    user (id = 2) -> r5                  

-}


spec_rbac :: Spec
spec_rbac = describeHasql [migrate] (pure fill) "rbac" $ 
  do itHasql "test1" test1
     itHasql "test2" test2
     itHasql "test3" test3
     itHasql "test4" test4
     itHasql "test5" test5

fill :: Session ()
fill = sql 
  [i|insert into "edgeNode"."Role" (title) values ('r1');
     insert into "edgeNode"."Role" (title, parent) values ('r2', 1);
     insert into "edgeNode"."Role" (title, parent) values ('r5', 1);
     insert into "edgeNode"."Role" (title, parent) values ('r3', 2);
     insert into "edgeNode"."Role" (title, parent) values ('r4', 2);
     insert into "edgeNode"."Role" (title, parent) values ('r6', 3);
     insert into "edgeNode"."Role" (title, parent) values ('r7', 3);
     insert into "edgeNode"."Role" (title, parent) values ('r8', 6);
     insert into "edgeNode"."Permission" (title) values ('root');
     insert into "edgeNode"."Permission" (title, parent) values ('provider_admin', 1);
     insert into "edgeNode"."Permission" (title, parent) values ('provider_guest', 2);
     insert into "edgeNode"."RolePermission" (rolefk, permissionfk) values (6, 1);
     insert into "edgeNode"."RolePermission" (rolefk, permissionfk) values (3, 1);
     insert into "edgeNode"."RolePermission" (rolefk, permissionfk) values (6, 2);
     insert into "edgeNode"."RolePermission" (rolefk, permissionfk) values (2, 3);
     insert into "edgeNode"."RolePermission" (rolefk, permissionfk) values (4, 1);
     insert into "edgeNode"."User" ("userGender") values ('male');
     insert into "edgeNode"."User" ("userGender") values ('male');
     insert into "edgeNode"."User" ("userGender") values ('male');
     insert into "edgeNode"."User" ("userGender") values ('male');
     insert into "edgeNode"."User" ("userGender") values ('male');
     insert into "edgeNode"."UserRole" (userfk, rolefk) values (1, 3);
     insert into "edgeNode"."UserRole" (userfk, rolefk) values (2, 3);
     insert into "edgeNode"."UserRole" (userfk, rolefk) values (3, 2);
     insert into "edgeNode"."UserRole" (userfk, rolefk) values (4, 8);
  |]

test1 :: Session ()
test1 = 
  do 
    xs <- 
      statement 
      (UserId 1, Root) 
      Rbac.getTopLevelRoles
    r <- statement (xs, Root) Rbac.elem
    r `shouldBe`Just True

test2 :: Session ()
test2 = 
  do 
    xs <- 
      statement 
      (UserId 1, ProviderAdmin) 
      Rbac.getTopLevelRoles
    r <- statement (xs, ProviderAdmin) Rbac.elem 
    r `shouldBe`Just True

test3 :: Session ()
test3 = 
  do 
    xs <- 
      statement 
      (UserId 3, ProviderGuest) 
      Rbac.getTopLevelRoles
    r <- statement (xs, ProviderGuest) Rbac.elem
    r `shouldBe`Just True
    
test4 :: Session ()
test4 = 
  do 
    xs <- 
      statement 
      (UserId 4, ProviderGuest) 
      Rbac.getTopLevelRoles
    r <- statement (xs, ProviderGuest) Rbac.elem 
    r `shouldBe`Just False

test5 :: Session ()
test5 = 
  do 
    xs <- 
      statement 
      (UserId 5, ProviderGuest) 
      Rbac.getTopLevelRoles
    r <- statement (xs, ProviderGuest) Rbac.elem
    r `shouldBe` (Just False)