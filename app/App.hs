{-# LANGUAGE TemplateHaskell #-}

module App (main) where

import qualified Application                     as App
import           KatipController
import           BuildInfo                       (gitLatestCommitHash)
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
import           Control.Monad
import           Data.Time.Clock (getCurrentTime)
import           System.Remote.Monitoring

main :: IO ()
main =
    do
      (cfgPath:_) <- getArgs
      cfg <- Config.load cfgPath
      pPrint cfg 

      void $ forkServer (cfg^.ekg.host.stext.textbs) (cfg^.ekg.port)

      term <- hGetTerm stdout
      orm <- createPostgresqlPool (mkOrmConn (cfg^.db)) (cfg^.orm.coerced)
      raw <- Hasql.acquire (cfg^.raw.poolN, cfg^.raw.tm, mkRawConn (cfg^.db))  
      let appEnv = KatipEnv term orm raw
      std <- mkHandleScribeWithFormatter 
             jsonFormat 
             ColorIfTerminal 
             stdout
             (cfg^.katip.severity.from stringify) 
             (cfg^.katip.verbosity.from stringify)
      tm <- getCurrentTime
      let katipFilePath = cfg^.katip.path <> "/" <> show tm <> ".log"
      file <- mkFileScribe 
              katipFilePath 
              (cfg^.katip.severity.from stringify) 
              (cfg^.katip.verbosity.from stringify)  
      let mkNm = Namespace [("<" ++ $(gitLatestCommitHash) ++ ">")^.stext]
      env <- initLogEnv mkNm (cfg^.katip.Config.env.stext.coerced)
      let env' = registerScribe "stdout" std defaultScribeSettings env >>= 
                 registerScribe "file" file defaultScribeSettings
      let runApp le = 
            runKatipContextT le 
            (mempty :: LogContexts) 
            mempty 
            (App.run (cfg^.ports.port))
      Migration.run orm
      bracket env' closeScribes ((`runReaderT` appEnv) . runApp)       
      
mkOrmConn :: Db -> String
mkOrmConn x = printf "host=%s port=%d dbname=%s user=%s password=%s" (x^.host) (x^.port) (x^.database) (x^.user) (x^.pass)

mkRawConn :: Db -> HasqlConn.Settings
mkRawConn x = 
  HasqlConn.settings
  (x^.host.stext.textbs)
  (x^.port.to fromIntegral)
  (x^.user.stext.textbs)
  (x^.pass.stext.textbs)
  (x^.database.stext.textbs)