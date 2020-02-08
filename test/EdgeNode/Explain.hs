{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE StandaloneDeriving #-}

module EdgeNode.Explain (spec_explain) where

import qualified EdgeNode.Statement.Rbac 
import qualified EdgeNode.Statement.File
import qualified EdgeNode.Statement.User
import qualified EdgeNode.Statement.Admin
import qualified EdgeNode.Statement.Provider

import Database.Migration.Test
import Test.Hspec hiding (shouldBe)
import Test.Hspec.DB.Hasql
import Hasql.Session
import Hasql.Statement
import Hasql.Decoders
import Data.Foldable
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Control.Monad.IO.Class
import Test.Hspec.Expectations.Lifted 
import Data.Generics.Product.Positions
import Control.Lens
import GHC.Generics

spec_explain :: Spec
spec_explain = 
  describeHasql 
  [migrate]
  Nothing 
  "explain" $
    for_ explainTests
    $ \(modl, tests) ->
        context modl
        $ for_ tests
        $ \(name, SomeQuery st) -> 
            itHasql name $ do 
              let st' = 
                    st & position @1 %~ ("explain " <>) 
                       & position @3 .~ noResult
              input <- liftIO $ generate arbitrary
              statement input st' >>= (`shouldBe` ())
  
deriving instance Generic (Statement a b)

-- | Existential wrapper for the query
data SomeQuery = forall a b . Arbitrary a => SomeQuery (Statement a b) 
     
-- | List of all database queries.
explainTests :: [(String, [(String, SomeQuery)])]
explainTests = 
  [ "EdgeNode.Statement.Rbac"
    ==> [ "getTopLevelRoles" =>> 
          EdgeNode.Statement.Rbac.getTopLevelRoles
        , "isPermissionBelongToRole" =>> 
          EdgeNode.Statement.Rbac.isPermissionBelongToRole
        , "assignRoleToUser" =>>
          EdgeNode.Statement.Rbac.assignRoleToUser  
        ]
  , "EdgeNode.Statement.File" ==> 
    [ "save" =>> EdgeNode.Statement.File.save
    , "getMeta" =>> EdgeNode.Statement.File.getMeta
    , "delete" =>> EdgeNode.Statement.File.delete
    , "getHashWithBucket" =>> EdgeNode.Statement.File.getHashWithBucket
    , "patch" =>> EdgeNode.Statement.File.patch
    ]
  , "EdgeNode.Statement.User" ==> ["new" =>> EdgeNode.Statement.User.new]
  , "EdgeNode.Statement.Admin" ==> ["newProvider" =>> EdgeNode.Statement.Admin.newProvider]
  , "EdgeNode.Statement.Provider" ==>
    [ "getBranches" =>> EdgeNode.Statement.Provider.getBranches
    , "checkHQ" =>> EdgeNode.Statement.Provider.checkHQ
    , "createBranches" =>> EdgeNode.Statement.Provider.createBranches]
  ]
  
(==>) a b = (a, b)
(=>>) a b = (a, SomeQuery b)