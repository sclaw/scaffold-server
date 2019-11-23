module EdgeNode.Statement.Rbac.Test (spec_rbac) where

import EdgeNode.Statement.Rbac
import EdgeNode.Model.User
import EdgeNode.Model.Rbac

import Database.Migration.Test
import Test.Hspec hiding (shouldBe)
import Test.Hspec.DB.Hasql
import Hasql.Session
import Test.Hspec.Expectations.Lifted

spec_rbac :: Spec
spec_rbac = describeHasql [migrate] (pure fill) "rbac" $ itHasql "" test

fill :: Session ()
fill = pure ()

test :: Session ()
test = statement (UserId 1, Root) check >>= (`shouldBe` True)