{-# LANGUAGE TemplateHaskell #-}

module App (main) where

import           Application
import           KatipController
import           BuildInfo                       (protoHash)
import           Config
import           Pretty
import qualified Database.Migration              as Migration

import           Control.Exception               (bracket)
import           Control.Lens                    
import           Control.Monad.Trans.Reader      (runReaderT)
import           Data.Monoid.Colorful            (hGetTerm)
import           Database.Groundhog.Postgresql   (createPostgresqlPool)
import           Katip
import           System.IO                       (stdout)
import qualified Hasql.Pool as                   Hasql
import qualified Hasql.Connection as             HasqlConn
import           System.Environment              (getArgs)
import           Text.Printf 
import           Control.Lens.Iso.Extended
import           Control.Concurrent.Async.Extended
import           Control.Monad
import           Control.Concurrent
import           Data.Time.Clock (getCurrentTime)

main :: IO ()
main =
    do
      (path:_) <- getArgs
      cfg <- Config.load path
      pPrint cfg 
      term <- hGetTerm stdout
      orm <- createPostgresqlPool (mkOrmConn (cfg^.db)) (cfg^.orm.coerced)
      raw <- Hasql.acquire (cfg^.raw.poolN, cfg^.raw.tm, mkRawConn (cfg^.db))  
      let appEnv = KatipEnv term orm raw
      std <- mkHandleScribeWithFormatter jsonFormat ColorIfTerminal stdout DebugS V3
      tm <- getCurrentTime
      let katipFilePath = cfg^.katip.coerced <> "/" <> show tm <> ".log"
      file <- mkFileScribe katipFilePath DebugS V3  
      let mkNm = Namespace [("<" ++ $(protoHash) ++ ">")^.stextiso]
      env <- initLogEnv mkNm "production"
      let env' = registerScribe "stdout" std defaultScribeSettings env >>= 
                 registerScribe "file" file defaultScribeSettings
      let runApp le = 
            runKatipContextT le 
            (mempty :: LogContexts) 
            mempty 
            (run (cfg^.ports.port))
      Migration.run orm
      let runServer = bracket env' closeScribes ((`runReaderT` appEnv) . runApp)       
      raceNThread [ runServer, interchangeWithServer ]

mkOrmConn :: Db -> String
mkOrmConn x = printf "host=%s port=%d dbname=%s user=%s password=%s" (x^.host) (x^.port) (x^.database) (x^.user) (x^.pass)

mkRawConn :: Db -> HasqlConn.Settings
mkRawConn x = 
  HasqlConn.settings
  (x^.host.stextiso.textbsiso)
  (x^.port.to fromIntegral)
  (x^.user.stextiso.textbsiso)
  (x^.pass.stextiso.textbsiso)
  (x^.database.stextiso.textbsiso)

interchangeWithServer :: IO ()
interchangeWithServer = forever $ do threadDelay 1000000; return ()