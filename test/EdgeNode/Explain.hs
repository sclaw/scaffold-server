{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Explain (spec_explain) where

import qualified EdgeNode.Statement.User as User

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

spec_explain :: Spec
spec_explain = 
  describeHasql 
  (map (statement ()) migrate) 
  Nothing 
  "explain" $
    for_ explainTests
    $ \(modl, tests) ->
        context modl
        $ for_ tests
        $ \(name, SomeQuery (Statement sql encoder _ _)) -> 
            itHasql name $ do 
              let st = Statement ("explain " <> sql) encoder noResult False
              input <- liftIO $ generate arbitrary
              r <- statement input st
              r `shouldBe` ()
  
-- | Existential wrapper for the query
data SomeQuery = forall a b . Arbitrary a => SomeQuery (Statement a b)

-- | List of all database queries.
explainTests :: [(String, [(String, SomeQuery)])]
explainTests = 
  [ "EdgeNode.Model.User"
    ==> [ "deleteQualification" =>> 
          User.deleteQualification
        , "deleteTrajectory" =>> 
          User.deleteTrajectory]
  ]
  
(==>) a b = (a, b)
(=>>) a b = (a, SomeQuery b)