{-# LANGUAGE TemplateHaskell #-}

module App (main) where

import           Application
import           KatipHandler
import           BuildInfo                       (protoHash)
import           Config
import           Pretty

import           Control.Exception               (bracket)
import           Control.Lens                    
import           Control.Lens.Iso.Extended       (stextiso)
import           Control.Monad.Trans.Reader      (runReaderT)
import           Data.Monoid.Colorful            (hGetTerm)
import           Database.Groundhog.Postgresql   (createPostgresqlPool)
import           Katip
import           System.IO                       (stdout)
import qualified Data.Pool  as                   Pool
import qualified Hasql.Pool as                   Hasql
import qualified Hasql.Connection as             HasqlConn
import           System.Environment              (getArgs)
import           Text.Printf 
import           Control.Lens.Iso.Extended

main :: IO ()
main =
    do
      (path:_) <- getArgs
      cfg <- Config.load path
      pPrint cfg 
      term <- hGetTerm stdout
      orm <- createPostgresqlPool (mkOrmConn (cfg^.db)) (cfg^.orm.coerced)
      let hasqlInit = Hasql.acquire (cfg^.raw.poolN, cfg^.raw.tm, mkRawConn (cfg^.db))
      raw <- Pool.createPool 
             hasqlInit 
             Hasql.release 
             (cfg^.pool.stripesN) 
             (cfg^.pool.timeToOpen) 
             (cfg^.pool.resPerStripe)  
      let appEnv = KatipEnv term orm raw
      std <- mkHandleScribe ColorIfTerminal stdout DebugS V3
      let mkNm = Namespace [("<" ++ $(protoHash) ++ ">")^.stextiso]
      env <- initLogEnv mkNm "production"
      let env' = registerScribe "stdout" std defaultScribeSettings env
      let runApp le = 
            runKatipContextT le 
            (mempty :: LogContexts) 
            mempty 
            (run (cfg^.ports.port))
      bracket env' closeScribes ((`runReaderT` appEnv) . runApp)

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