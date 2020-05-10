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
{-# LANGUAGE FlexibleContexts #-}

module App (main) where

import qualified EdgeNode.Application as App
import EdgeNode.Config
import EdgeNode.Transport.Id

import KatipController
import BuildInfo (gitCommit)
import Pretty
import qualified Database.Migration as Migration
import Control.Lens hiding (Wrapped, Unwrapped)
import Data.Monoid.Colorful (hGetTerm)
import Katip
import qualified Hasql.Connection as HasqlConn
import Control.Lens.Iso.Extended
import Control.Monad
import Data.Time.Clock (getCurrentTime)
import Data.Aeson (eitherDecode)
import Data.Either.Combinators
import qualified Data.ByteString.Lazy  as B
import Data.Default.Class
import Control.Monad.RWS.Strict (evalRWST)
import qualified Network.HTTP.Client.TLS as Http
import Network.HTTP.Client
       ( ManagerSettings
         ( managerConnCount
         , managerResponseTimeout)
       , responseTimeoutMicro)
import Options.Generic
import Data.Maybe
import System.FilePath.Posix
import Control.Monad.IO.Class
import qualified Data.Pool as Pool
import Control.Exception
import qualified Network.Minio as Minio
import Data.String
import Data.Int
import System.IO
import qualified Web.Telegram as Web.Telegram

data Cmd w =
     Cmd
     { cfgPath :: w ::: FilePath <?> "config file path"
     , localhost :: w ::: Maybe String <?> "override db host if needed, used along with port"
     , localport :: w ::: Maybe Int <?> "override db port if needed"
     , isAuth :: w ::: Maybe Bool <?> "is auth turn on"
     , pathToKatip :: w ::: Maybe FilePath <?> "path to katip log"
     , pathToJwk :: w ::: Maybe FilePath <?> "path to jwk"
     , minioHost :: w ::: Maybe String <?> "minio host"
     , minioPort :: w ::: Maybe String <?> "minio port"
     , user :: w ::: Maybe Int64 <?> "user"
     , swaggerHost :: w ::: Maybe String <?> "swagger host"
     , swaggerPort :: w ::: Maybe Int <?> "swagger port"
     , serverPort :: w ::: Maybe Int <?> "server port"
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
            & auth.isAuthEnabled %~ (`fromMaybe` isAuth)
            & auth.userId %~ (`fromMaybe` user)
            & katip.path %~ (\path -> maybe path (</> path) pathToKatip)
            & auth.EdgeNode.Config.jwk %~ (\path -> maybe path (</> path) pathToJwk)
            & EdgeNode.Config.minio.host %~ (`fromMaybe` minioHost)
            & EdgeNode.Config.minio.port %~ (`fromMaybe` minioPort)
            & swagger.host %~ (`fromMaybe` swaggerHost)
            & swagger.port %~ (`fromMaybe` swaggerPort)
            & serverConnection.port %~ (`fromMaybe` serverPort)
      pPrint cfg

      term <- hGetTerm stdout
      hSetBuffering stdout NoBuffering

      hasqlpool <- Pool.createPool
        (HasqlConn.acquire (mkRawConn (cfg^.db)) >>=
         either (throwIO . ErrorCall . maybe "hasql conn error" (^.from textbs.from stext)) pure)
        HasqlConn.release
        (cfg^.hasql.poolN)
        (cfg^.hasql.tm)
        (cfg^.hasql.resPerStripe)

      std <- mkHandleScribeWithFormatter
             jsonFormat
             ColorIfTerminal
             stdout
             (permitItem (cfg^.katip.severity.from stringify))
             (cfg^.katip.verbosity.from stringify)
      tm <- getCurrentTime
      let katipFilePath = cfg^.katip.path <> "/" <> show tm <> ".log"
      fileHdl <- openFile katipFilePath AppendMode

      mapM_ (`hSetEncoding` utf8) [stdout, stderr, fileHdl]

      file <- mkHandleScribe
              (ColorLog True)
              fileHdl
              (permitItem (cfg^.katip.severity.from stringify))
              (cfg^.katip.verbosity.from stringify)
      let mkNm = Namespace [("<" ++ $(gitCommit) ++ ">")^.stext]
      init_env <- initLogEnv mkNm (cfg^.katip.EdgeNode.Config.env.isoEnv.stext.coerced)
      let env = do
            env' <- registerScribe "stdout" std defaultScribeSettings init_env
            registerScribe "file" file defaultScribeSettings env'

      jwke <- eitherDecode `fmap` B.readFile (cfg^.auth.EdgeNode.Config.jwk)
      jwke `whenLeft` (error . (<>) "jwk decode error: ")
      let appCfg =
           App.Cfg
           (cfg^.swagger.host.coerced)
           (cfg^.swagger.port)
           (cfg^.serverConnection.port)
           (fromRight' jwke)
           (cfg^.auth.isAuthEnabled)
           (cfg^.auth.userId.coerced)
      let runApp le =
           runKatipContextT le (mempty :: LogContexts) mempty $
            do logger <- katipAddNamespace (Namespace ["db", "migration"]) askLoggerIO
               liftIO $ Migration.run hasqlpool logger
               App.run appCfg
      manager <- Http.newTlsManagerWith Http.tlsManagerSettings
        { managerConnCount = 1
        , managerResponseTimeout =
          responseTimeoutMicro (5 * 10^6) }

      minioEnv <-  flip Minio.mkMinioConn manager $
        Minio.setCreds
        (Minio.Credentials
         (cfg^.EdgeNode.Config.minio.accessKey)
         (cfg^.EdgeNode.Config.minio.secretKey))
        (fromString (cfg^.EdgeNode.Config.minio.host <> ":" <> cfg^.EdgeNode.Config.minio.port))

      telegram <- Web.Telegram.mkService manager (cfg^.EdgeNode.Config.telegram)

      let katipMinio = Minio minioEnv (cfg^.EdgeNode.Config.minio.EdgeNode.Config.bucketPrefix)
      let katipEnv = KatipEnv term hasqlpool manager (cfg^.service.coerced) (fromRight' jwke) katipMinio telegram

      bracket env closeScribes (void . (\x -> evalRWST (App.runAppMonad x) katipEnv def) . runApp)

mkRawConn :: Db -> HasqlConn.Settings
mkRawConn x = HasqlConn.settings (x^.host.stext.textbs) (x^.port.to fromIntegral) (x^.EdgeNode.Config.user.stext.textbs) (x^.pass.stext.textbs) (x^.database.stext.textbs)