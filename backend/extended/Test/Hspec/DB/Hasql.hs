{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Test.Hspec.DB.Hasql (itHasql, describeHasql, session) where

import Control.Exception (bracketOnError, finally)
import Control.Monad
import qualified Data.ByteString.Char8  as Char8          
import Data.Foldable
import qualified Database.Postgres.Temp as Temp
import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as Hasql
import Test.Hspec as HSpec
import Control.Lens
import Control.Applicative ((<|>))

data TestDBHasql = 
     TestDBHasql
     { tempDB :: !Temp.DB
       -- ^ handle for temporary @postgres@ process
     , conn   :: !Hasql.Connection
       -- ^ connections to the temporary @postgres@
     }

-- | Start a temporary postgres process and create a connections to it.
setupDBHasql
  :: Hasql.Session ()
  -> Maybe (Hasql.Session ())
     -- ^ Data population.
  -> IO TestDBHasql
setupDBHasql migration mpopulate =
  bracketOnError
      (Temp.start [] >>= \case
        Left e -> error $ "Error during db initialization: " <> show e
        Right x -> pure x
      )
      Temp.stop
    $ \tempDB -> do
        let connStr = Char8.pack (Temp.connectionString tempDB)
        bracketOnError
            (Hasql.acquire connStr >>= \case
              Left e -> error $ "Error connecting to db" <> show e
              Right conn -> pure conn
            )
            Hasql.release
          $ \conn -> do
              migr <- migration `Hasql.run` conn
              pop <- traverse (`Hasql.run` conn) mpopulate
              traverse_
                (error . ("error while populating db: " <>) . show)
                (Just migr ^? _Just . _Left <|> pop ^? _Just . _Left)
              pure TestDBHasql {..}

-- | Tear down DB Hasql.
tearDownDBHasql :: TestDBHasql -> IO ()
tearDownDBHasql TestDBHasql {..} = Hasql.release conn `finally` void (Temp.stop tempDB)

-- | Run session helper, throws a error, in session
-- errors out.
runSession :: Hasql.Session a -> TestDBHasql -> IO a
runSession action db = (action `Hasql.run` conn db) >>= either (error . show) return

-- | Wrapper for hasql test.
itHasql :: String -> Hasql.Session a -> SpecWith TestDBHasql
itHasql str action = str `HSpec.it` (void . runSession action)

-- | Run a hasql session bases test. Test takes a function that
-- can run the `Session` in on the current connection.
session :: String -> ((forall a . Hasql.Session a -> IO (Either Hasql.QueryError a)) -> IO a) -> SpecWith TestDBHasql
session name f = HSpec.it name (void . test)
  where test db = f (`Hasql.run` conn db)

-- | Hasql test.
describeHasql
  :: Hasql.Session () -- ^ Database initialization.
  -> Maybe (Hasql.Session ()) -- ^ Database population.
  -> String -- ^ Test name.
  -> SpecWith TestDBHasql -- ^ Test itself.
  -> Spec
describeHasql migration mpopulate str =
  beforeAll (setupDBHasql migration mpopulate) . afterAll tearDownDBHasql . HSpec.describe str