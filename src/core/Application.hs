{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Application (App, AppEnv (AppEnv), app) where

import           KatipHandler

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Monoid.Colorful          (Term)
import           Data.Pool                     (Pool)
import           Database.Groundhog.Postgresql (Postgresql)
import           Katip
import           Network.Wai.Handler.Warp      (run)
import           Servant


type App = ReaderT AppEnv IO

data AppEnv = AppEnv { appEnvTerm :: Term, appEnvCm :: Pool Postgresql }

type AppApi = "getAllIntegers" :> Get '[JSON] [Int]

appApi :: Proxy AppApi
appApi = Proxy

server :: ServerT AppApi KatipHandler
server = const [1, 2, 4] `fmap` $(logTM) DebugS "getAllIntegers"

app :: KatipContextT App ()
app =
    do
      $(logTM) DebugS "app run.."
      configCm <- appEnvCm `fmap` lift ask
      let initCfg =
           do
            configEnv <-  getLogEnv
            configCtx <- getKatipContext
            configNm <-  getKatipNamespace
            return $ Config {..}
      cfg <- initCfg
      let runKH =
             (`runReaderT` cfg)
           . runKatipHandler
      liftIO $ run 11000 (serve appApi (hoistServer appApi runKH server))