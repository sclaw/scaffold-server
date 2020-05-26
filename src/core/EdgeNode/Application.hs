{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Application (Cfg (..), AppMonad (..), run) where

import EdgeNode.Api
import qualified EdgeNode.Controller.Controller as Controller
import EdgeNode.Transport.Id

import Auth
import KatipController
import Servant.Swagger.KatipController
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Katip
import qualified Network.Wai.Handler.Warp      as Warp
import Servant
import Servant.API.Generic
import Control.Lens
import Servant.Swagger.UI
import Servant.Auth.Server
import Crypto.JOSE.JWK
import Control.Concurrent.Async
import Network.Wai
import Control.Lens.Iso.Extended
import qualified Middleware
import Control.Monad.RWS.Strict as RWS
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Network.Wai.Middleware.Cors
import qualified Hasql.Connection as Hasql
import qualified Data.Pool as Pool
import Data.Generics.Product.Fields
import Servant.Multipart
import Network.Wai.Parse
import Network.HTTP.Types.Status
import TextShow
import BuildInfo
import qualified Network.Wai.Handler.WarpTLS as Warp
import System.Directory
import System.FilePath.Posix
import Control.Concurrent.Lifted
import Pretty
import Data.Either.Combinators
import Control.Exception

data Cfg =
     Cfg
     { cfgHost         :: !String
     , cfgSwaggerPort  :: !Int
     , cfgServerPort   :: !Int
       -- JSON Web Key (JWK) is a JavaScript Object Notation (JSON)
       -- data structure that represents a cryptographic key
     , cfgJwk           :: !JWK
     , cfgIsAuthEnabled :: !Bool
     , cfgUserId        :: !(Id "user")
     }

newtype AppMonad a = AppMonad { runAppMonad :: RWS.RWST KatipEnv KatipLogger KatipState IO a }
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad
  deriving newtype MonadIO
  deriving newtype (MonadReader KatipEnv)
  deriving newtype (MonadState KatipState)
  deriving newtype (MonadWriter KatipLogger)
  deriving newtype (MonadRWS KatipEnv KatipLogger KatipState)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype MonadCatch
  deriving newtype MonadThrow

run :: Cfg -> KatipContextT AppMonad ()
run Cfg {..} =
  katipAddNamespace (Namespace ["application"]) $
    do
      telegram_service <- fmap (^.telegram) ask
      let runTelegram l msg = void $ fork $ liftIO $ send telegram_service l ((mkPretty ("At module " <> $location) msg)^.stext)
      logger <- katipAddNamespace (Namespace ["application"]) askLoggerIO

      version_e <- liftIO getVersion
      runTelegram logger $ "server version " <> show version_e
      whenLeft version_e $ \e -> throwM $ ErrorCall e
      let Right ver = version_e

      runTelegram logger $ "server run on port " <> show cfgServerPort

      $(logTM) DebugS $ ls $ "server run on port " <> showt cfgServerPort
      configKatipEnv <- lift ask
      let initCfg =
           do
             configEnv <- getLogEnv
             configCtx <- getKatipContext
             configNm <-  getKatipNamespace
             return $ Config {..}
      cfg <- initCfg
      let withSwagger :: Proxy a -> Proxy (a :<|> SwaggerSchemaUI "swagger" "swagger.json")
          withSwagger _ = Proxy
      let jwtCfg = defaultJWTSettings cfgJwk
      let context
           :: Proxy
              '[ CookieSettings
               , JWTSettings
               , KatipLoggerIO
               , Bool
               , Id "user"
               , Proxy Tmp
               , Pool.Pool Hasql.Connection
               , BasicAuthCfg]
          context = Proxy
      let server =
           hoistServerWithContext
           (withSwagger api)
           context
           (runKatipController cfg (KatipControllerState 0))
           (toServant Controller.controller :<|>
            swaggerSchemaUIServerT (swaggerHttpApi cfgHost cfgSwaggerPort ver))
      excep <-katipAddNamespace (Namespace ["exception"]) askLoggerIO
      ctxlog <-katipAddNamespace (Namespace ["context"]) askLoggerIO
      let settings =
           Warp.defaultSettings
           & Warp.setPort cfgServerPort
           & Warp.setOnException (logUncaughtException excep runTelegram)
           & Warp.setOnExceptionResponse mkResponse
           & Warp.setServerName ("edgenode api server, revision " <> $gitCommit)
      let multipartOpts =
            (defaultMultipartOptions (Proxy :: Proxy Tmp))
            { generalOptions = clearMaxRequestNumFiles defaultParseRequestBodyOptions }
      let mkCtx =    jwtCfg
                  :. defaultCookieSettings
                  :. ctxlog
                  :. cfgIsAuthEnabled
                  :. cfgUserId
                  :. (cfg^.katipEnv.hasqlDbPool)
                  :. multipartOpts
                  :. (BasicAuthCfgData (cfg^.katipEnv.hasqlDbPool) ctxlog)
                  :. EmptyContext
      let runServer = serveWithContext (withSwagger api) mkCtx server
      mware <-katipAddNamespace (Namespace ["middleware"]) askLoggerIO

      path <- liftIO getCurrentDirectory
      let tls_settings = (Warp.tlsSettings (path </> "tls/certificate") (path </> "tls/key")) { Warp.onInsecure = Warp.AllowInsecure }

      servAsync <- liftIO $ async $ Warp.runTLS tls_settings settings (middleware mware runServer)
      liftIO (void (waitAnyCancel [servAsync])) `logExceptionM` ErrorS

middleware :: KatipLoggerIO -> Application -> Application
middleware log app = mkCors $ Middleware.logger log app

logUncaughtException :: KatipLoggerIO -> (KatipLoggerIO -> String -> IO ()) -> Maybe Request -> SomeException -> IO ()
logUncaughtException log runTelegram req e = when (Warp.defaultShouldDisplayException e) $ maybe without within req
  where without = do
          runTelegram log $ "Uncaught exception " <> show e
          log ErrorS (logStr ("Uncaught exception " <> show e))
        within r = do
          runTelegram log $ "\"GET " <> rawPathInfo r^.from textbs.from stext <> " HTTP/1.1\" 500 - " <> show e
          log ErrorS (logStr ("\"GET " <> rawPathInfo r^.from textbs.from stext <> " HTTP/1.1\" 500 - " <> show e))

mkResponse :: SomeException  -> Response
mkResponse error = responseLBS status500 [("Access-Control-Allow-Origin", "http://edgenode.org")] (showt error^.textbsl)

deriving instance Generic CorsResourcePolicy

mkCors :: Middleware
mkCors =
  cors $ const $ pure $
    simpleCorsResourcePolicy
    & field @"corsOrigins" .~ Just (["http://edgenode.org"], True)
    & field @"corsRequestHeaders" .~
      [ "Authorization"
      , "Content-Type"
      , "Origin"]
    & field @"corsExposedHeaders" ?~
      ["X-Set-Bearer"]
    & field @"corsMethods" .~
      simpleMethods <>
      ["PUT", "PATCH", "DELETE", "OPTIONS"]