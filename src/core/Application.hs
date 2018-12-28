{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Application (App, AppEnv (AppEnv), app) where

import           Api
import qualified Controller.Controller as Controller
import           KatipHandler

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Monoid.Colorful          (Term)
import           Data.Pool                     (Pool)
import           Data.Swagger
import           Database.Groundhog.Postgresql (Postgresql)
import           Katip
import           Network.Wai.Handler.Warp      (run)
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI

type App = ReaderT AppEnv IO
type AppSwagger = Proxy AppApi -> Proxy (AppApi :<|> SwaggerSchemaUI "swagger" "swagger.json")

data AppEnv = AppEnv { appEnvTerm :: Term, appEnvCm :: Pool Postgresql }

app :: KatipContextT App ()
app =
    do
      $(logTM) DebugS "app run.."
      configCm <- appEnvCm `fmap` lift ask
      let initCfg =
           do
            configEnv <- getLogEnv
            configCtx <- getKatipContext
            configNm <-  getKatipNamespace
            return $ Config {..}
      cfg <- initCfg
      let runKH =
             (`runReaderT` cfg)
           . runKatipHandler
      let swaggerServer =
           hoistServer appApi runKH Controller.execQuery :<|>
           swaggerSchemaUIServer appSwagger
          swaggerApi :: AppSwagger
          swaggerApi _ = Proxy
      liftIO $ run 11000 (serve (swaggerApi appApi) swaggerServer)

appSwagger :: Swagger
appSwagger =
    toSwagger appApi
    & info.title   .~ "web server"
    & info.description ?~ ""
    & info.version .~ "0.0.1"
