{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Migration (run) where

import Data.Word
import Data.Bool (bool)
import Data.Time.Clock ()
import Katip
import Control.Lens 
import Control.Lens.Iso.Extended
import Data.String.Interpolate
import Data.Foldable
import qualified Database.Migration.Batch as Batch
import Data.Maybe
import Control.Monad
import Control.Applicative
import KatipController
import qualified Hasql.Pool
import qualified Hasql.Session
import qualified Hasql.Statement
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Hasql.Pool as Hasql
import Prelude hiding (init)
import Control.Monad.IO.Class

run :: Hasql.Pool -> KatipLoggerIO -> IO (Either Hasql.Pool.UsageError ())
run cm logger = Hasql.Pool.use cm $ Hasql.Session.statement () checkDBMeta >>= traverse_ (bool (initDb logger) (roll Nothing logger))

checkDBMeta :: Hasql.Statement.Statement () (Maybe Bool)
checkDBMeta = Hasql.Statement.Statement sql HE.noParams decoder False
  where 
    sql =
      [i|select exists (select 1
         from information_schema.tables 
         where table_schema = 'public'
               and table_name = 'db_meta')|]
    decoder = HD.rowMaybe $ HD.column (HD.nonNullable HD.bool)

initDb :: KatipLoggerIO -> Hasql.Session.Session ()
initDb logger = Hasql.Session.statement () mkDbMeta >> roll (Just 1) logger

mkDbMeta = Hasql.Statement.Statement sql HE.noParams HD.noResult False
  where 
    sql = 
     [i|create table db_meta (
        version int4 not null,
        created timestamptz not null default now(),
        modified timestamptz)|]

roll :: Maybe Word32 -> KatipLoggerIO -> Hasql.Session.Session () 
roll v logger =
  do
    v' <- (<|> v) `fmap` Hasql.Session.statement () getVersion
    let batch = Batch.Version `fmap` v
    for_ (v' <|> v) $ \i -> do
      liftIO $ logger InfoS (logStr ("migration will be start from version " <> i^.stringify))     
      next <- Batch.exec (Batch.Version i) logger
      for_ (next <|> batch) $ \ident -> do
        let new = ident^.coerced 
        Hasql.Session.statement () (setVersion (maybe (New new) (const (Init new)) v))
        liftIO $ logger InfoS (logStr ("migration finished at version " <> show ident))
      when (isNothing next) $ liftIO $ logger InfoS (logStr ("no migration found" :: String))

getVersion :: Hasql.Statement.Statement () (Maybe Word32)
getVersion = Hasql.Statement.Statement sql HE.noParams decoder False
  where sql = [i|select version from db_meta|]
        decoder = HD.rowMaybe $ HD.column (HD.nonNullable (HD.int4 <&> (^.integral)))

data SetVrsion = Init Word32 | New Word32   

setVersion :: SetVrsion -> Hasql.Statement.Statement () ()
setVersion (Init v) = Hasql.Statement.Statement sql encoder HD.noResult False
  where sql = [i|insert into db_meta (version, created) values ($1, now())|]
        encoder = v^.integral >$ HE.param (HE.nonNullable HE.int4)
setVersion (New v) = Hasql.Statement.Statement sql encoder HD.noResult False
  where sql = [i|update db_meta set version = $1, modified = now()|]
        encoder = v^.integral >$ HE.param (HE.nonNullable HE.int4)