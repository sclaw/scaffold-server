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
import           Control.Concurrent.MVar (MVar, takeMVar)
import           Control.Concurrent.Async.Lifted (race_)
import           Katip.Core                      (ScribeHandle (shScribe))
import           System.FilePath.Posix           (FilePath)
import qualified Data.Map.Strict                 as M
import           Control.Lens                    ((%~), (&))


type App = ReaderT AppEnv IO

data AppEnv = AppEnv { appEnvTerm :: Term, appEnvCm :: Pool Postgresql }

type AppApi = "getAllIntegers" :> Get '[JSON] [Int]

appApi :: Proxy AppApi
appApi = Proxy

server :: ServerT AppApi KatipHandler
server = const [1, 2, 4] `fmap` $(logTM) DebugS "getAllIntegers"

app :: MVar String -> KatipContextT App ()
app var =
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
      race_ (modifyEnv var) (liftIO (run 11000 (serve appApi (hoistServer appApi runKH server))))

modifyEnv :: MVar FilePath -> KatipContextT App ()
modifyEnv var =
    do
      path <- liftIO $ takeMVar var
      file <- liftIO $ mkFileScribe path DebugS V2
      let replace = M.update (\x -> Just x { shScribe = file }) "file"
      localLogEnv (& logEnvScribes %~ replace) (modifyEnv var)