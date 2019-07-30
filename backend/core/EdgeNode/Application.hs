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

module EdgeNode.Application (Cfg (..), AppMonad (..), run) where

import EdgeNode.Api
import qualified EdgeNode.Controller.Application as App

import Katip.Monadic                 (askLoggerIO)
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
import Control.Monad (when, void)
import Network.Wai (Request, rawPathInfo)
import Control.Lens.Iso.Extended
import GHC.Exception.Type (SomeException) 
import qualified Middleware
import Control.Monad.RWS.Strict as RWS
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Trans.Control

data Cfg = 
     Cfg 
     { cfgHost :: !String 
     , cfgPort :: !Int
       -- JSON Web Key (JWK) is a JavaScript Object Notation (JSON) 
       -- data structure that represents a cryptographic key 
     , cfgJwk :: !JWK
     , cfgIsAuthEnabled :: !Bool 
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
      $(logTM) DebugS "application run.."
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
      let context :: Proxy '[CookieSettings, JWTSettings]
          context = Proxy     
      let server = 
           hoistServerWithContext 
           (withSwagger api)
           context
           (runKatipController cfg) 
           (toServant App.application :<|> 
            swaggerSchemaUIServerT (swaggerHttpApi cfgHost cfgPort))
      excep <-katipAddNamespace (Namespace ["exception"]) askLoggerIO
      let settings = 
           Warp.defaultSettings
           & Warp.setPort cfgPort
           & Warp.setOnException (logUncaughtException excep)
      let runServer = 
           serveWithContext 
           (withSwagger api) 
           (defaultCookieSettings :. jwtCfg :. EmptyContext) 
           server
      mware <-katipAddNamespace (Namespace ["middleware"]) askLoggerIO
      servAsync <- liftIO $ async $ Warp.runSettings settings (middleware mware runServer)     
      liftIO (void (waitAnyCancel [servAsync])) `logExceptionM` ErrorS
    
middleware :: KatipLoggerIO -> Application -> Application
middleware = Middleware.logger

logUncaughtException :: KatipLoggerIO -> Maybe Request -> SomeException -> IO ()     
logUncaughtException log req e = when (Warp.defaultShouldDisplayException e) $ maybe without within req
  where without = log ErrorS (logStr ("Uncaught exception " <> show e))
        within r = log ErrorS (logStr ("\"GET " <> rawPathInfo r^.from textbs.from stext <> " HTTP/1.1\" 500 - " <> show e))