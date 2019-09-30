{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module App (main) where

import qualified EdgeNode.Application as App
import EdgeNode.Config

import KatipController
import BuildInfo (gitLatestCommitHash)
import Pretty
import qualified Database.Migration as Migration
import Control.Exception (bracket)
import Control.Lens hiding (Wrapped, Unwrapped) 
import Data.Monoid.Colorful (hGetTerm)
import Database.Groundhog.Postgresql (createPostgresqlPool)
import Katip
import System.IO (stdout)
import qualified Hasql.Pool as Hasql
import qualified Hasql.Connection as HasqlConn
import Control.Lens.Iso.Extended
import Control.Monad
import Data.Time.Clock (getCurrentTime)
import System.Remote.Monitoring
import Data.Aeson (eitherDecode)
import Data.Either.Combinators
import qualified Data.ByteString.Lazy  as B
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Data.Default.Class
import Control.Monad.RWS.Strict (evalRWST)
import Data.String.Interpolate
import qualified Network.HTTP.Client.TLS as Http
import Network.HTTP.Client (ManagerSettings (managerConnCount))
import GHC.Generics (Generic)
import Options.Generic
import Data.Maybe
import System.FilePath.Posix

data Cmd w = 
     Cmd 
     { cfgPath :: w ::: FilePath <?> "config file path"
     , localhost :: w ::: Maybe String <?> "override db host if needed, used along with port"
     , localport :: w ::: Maybe Int <?> "override db port if needed" 
     , ekgHost :: w ::: Maybe String <?> "ekg host"
     , isAuth :: w ::: Maybe Bool <?> "is auth turn on"
     , pathToKatip :: w ::: Maybe FilePath <?> "path to katip log"
     , pathToJwk :: w ::: Maybe FilePath <?> "path to jwk"
     , swaggerHost :: w ::: Maybe FilePath <?> "swagger host"
     } deriving stock Generic
    
instance ParseRecord (Cmd Wrapped)
deriving instance Show (Cmd Unwrapped)

main :: IO ()
main =
    do
      Cmd {..} <- unwrapRecord "edgenode server"
      rawCfg <- EdgeNode.Config.load cfgPath
      let cfg = 
            rawCfg 
            & db.host %~ (`fromMaybe` localhost) 
            & db.port %~ (`fromMaybe` localport) 
            & ekg.host %~ (`fromMaybe` ekgHost)
            & auth.isAuthEnabled %~ (`fromMaybe` isAuth)
            & katip.path %~ (\path -> maybe path (</> path) pathToKatip)
            & auth.EdgeNode.Config.jwk %~ (\path -> maybe path (</> path) pathToJwk)
            & hosts %~ (`fromMaybe` fmap Hosts swaggerHost)
      pPrint cfg

      void $ forkServer (cfg^.ekg.host.stext.textbs) (cfg^.ekg.port)

      term <- hGetTerm stdout
      hSetBuffering stdout NoBuffering 

      orm <- createPostgresqlPool (mkOrmConn (cfg^.db)) (cfg^.orm.coerced)
      raw <- Hasql.acquire (cfg^.raw.poolN, cfg^.raw.tm, mkRawConn (cfg^.db))
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
      env <- initLogEnv mkNm (cfg^.katip.EdgeNode.Config.env.stext.coerced)
      let env' = registerScribe "stdout" std defaultScribeSettings env >>= 
                 registerScribe "file" file defaultScribeSettings

      jwke <- eitherDecode `fmap` B.readFile (cfg^.auth.EdgeNode.Config.jwk)
      jwke `whenLeft` (error . (<>) "jwk decode error: ")
      let appCfg = 
           App.Cfg 
           (cfg^.hosts.coerced) 
           (cfg^.ports.port) 
           (fromRight' jwke) 
           (cfg^.auth.isAuthEnabled)
           (cfg^.auth.userId._Unwrapped')                    

      let runApp le = 
           runKatipContextT le (mempty :: LogContexts) mempty $  
            do e <- Migration.run orm 
               let err e = error $ "migration failure, error: " <> show e
               either err (const (App.run appCfg)) e
      manager <- Http.newTlsManagerWith Http.tlsManagerSettings { managerConnCount = 1 }
      let katipEnv = KatipEnv term orm raw manager (cfg^.service.coerced) (fromRight' jwke)
      bracket env' closeScribes (void . (\x -> evalRWST (App.runAppMonad x) katipEnv def) . runApp)       
      
mkOrmConn :: Db -> String
mkOrmConn x = 
  [i| host=#{x^.host} port=#{x^.port} 
      dbname=#{x^.database} user=#{x^.user} 
      password=#{x^.pass}
  |]

mkRawConn :: Db -> HasqlConn.Settings
mkRawConn x = 
  HasqlConn.settings
  (x^.host.stext.textbs)
  (x^.port.to fromIntegral)
  (x^.user.stext.textbs)
  (x^.pass.stext.textbs)
  (x^.database.stext.textbs)