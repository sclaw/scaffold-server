module EdgeNode.Statement.Rbac (spec_rbac) where

import Database.Migration.Test
import Test.Hspec
import Test.Hspec.DB.Hasql
import Hasql.Session

spec_rbac :: Spec
spec_rbac = describeHasql [migrate] (pure fill) "rbac" undefined 

fill :: Session ()
fill = undefined