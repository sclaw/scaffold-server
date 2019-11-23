module EdgeNode.Statement.Rbac.Test (spec_rbac) where

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
test = shouldBe () ()