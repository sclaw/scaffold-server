module EdgeNode.Statement.Rbac (check) where

import EdgeNode.Model.User
import EdgeNode.Model.Rbac

import qualified Hasql.Statement as HS

check :: HS.Statement (UserId, Permission) Bool
check = undefined