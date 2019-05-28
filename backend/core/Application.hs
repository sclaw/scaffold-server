{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes         #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Application (App, run) where

import           Api
import qualified Controller.Application as App
import           KatipHandler

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Katip
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant                       hiding (Application)
import           Servant.API.Generic

type App = ReaderT KatipEnv IO

run :: KatipContextT App ()
run =
    do
      $(logTM) DebugS "app run.."
      configKatipEnv <- lift ask
      let initCfg =
           do
             configEnv <- getLogEnv
             configCtx <- getKatipContext
             configNm <-  getKatipNamespace
             return $ Config {..}
      cfg <- initCfg
      let runKH = (`runReaderT` cfg) . runKatipHandler
      let server = hoistServer api runKH (toServant (App.application)) 
      liftIO $ Warp.run 11000 (serve api server)