{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes         #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Application (run) where

import           Api
import qualified Controller.Application as App
import           KatipController
import           Servant.Swagger.KatipController

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Katip
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant                    
import           Servant.API.Generic
import           Control.Lens
import           Servant.Swagger.UI

run :: Int -> KatipContextT (ReaderT KatipEnv IO) ()
run port = 
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
      let server = 
           hoistServer 
           (withSwagger api) 
           ((`runReaderT` cfg) . runKatipController) 
           (toServant App.application :<|> 
            swaggerSchemaUIServerT (swaggerHttpApi port))
      let settings = 
           Warp.defaultSettings
           & Warp.setPort port
           & Warp.setOnException Warp.defaultOnException
      liftIO (Warp.runSettings settings (serve (withSwagger api) server)) `logExceptionM` ErrorS