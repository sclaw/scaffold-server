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
  [i|delete from auth.role_permission;
     delete from auth.role;
     insert into auth.role (title) values ('r1');
     insert into auth.role (title, parent_fk) select 'r2', id from auth.role where title = 'r1';
     insert into auth.role (title, parent_fk) select 'r5', id from auth.role where title = 'r1';
     insert into auth.role (title, parent_fk) select 'r3', id from auth.role where title = 'r2';
     insert into auth.role (title, parent_fk) select 'r4', id from auth.role where title = 'r2';
     insert into auth.role (title, parent_fk) select 'r6', id from auth.role where title = 'r5';
     insert into auth.role (title, parent_fk) select 'r7', id from auth.role where title = 'r5';
     insert into auth.role (title, parent_fk) select 'r8', id from auth.role where title = 'r6';
     insert into auth.role_permission (role_fk, permission_fk) select id, 1 from auth.role where title = 'r6';
     insert into auth.role_permission (role_fk, permission_fk) select id, 1 from auth.role where title = 'r3';
     insert into auth.role_permission (role_fk, permission_fk) select id, 2 from auth.role where title = 'r6';
     insert into auth.role_permission (role_fk, permission_fk) select id, 3 from auth.role where title = 'r2';
     insert into auth.role_permission (role_fk, permission_fk) select id, 1 from auth.role where title = 'r4';
     insert into auth.user (identifier, password, created, user_type) values ('uid1', '', now(), '');
     insert into auth.user (identifier, password, created, user_type) values ('uid2', '', now(), '');
     insert into auth.user (identifier, password, created, user_type) values ('uid3', '', now(), '');
     insert into auth.user (identifier, password, created, user_type) values ('uid4', '', now(), '');
     insert into auth.user (identifier, password, created, user_type) values ('uid5', '', now(), '');
     insert into auth.user_role (user_fk, role_fk) select 1, id from auth.role where title = 'r3';
     insert into auth.user_role (user_fk, role_fk) select 2, id from auth.role where title = 'r3';
     insert into auth.user_role (user_fk, role_fk) select 3, id from auth.role where title = 'r2';
     insert into auth.user_role (user_fk, role_fk) select 4, id from auth.role where title = 'r8';
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