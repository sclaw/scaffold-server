{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes         #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Application (App, app) where

import           Api
import qualified Controller.Controller as Controller
import           KatipHandler

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Katip
import           Network.Wai.Handler.Warp      (run)
import           Servant hiding (Application)
import           Servant.API.Generic

type App = ReaderT KatipEnv IO

app :: KatipContextT App ()
app =
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
      let server = hoistServer api runKH (toServant (Controller.execQuery)) 
      liftIO $ run 11000 (serve api server)