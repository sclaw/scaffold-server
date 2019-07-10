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

module EdgeNode.Application (Cfg (..), run) where

import           EdgeNode.Api
import qualified EdgeNode.Controller.Application as App

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
import           Servant.Auth.Server
import           Crypto.JOSE.JWK


data Cfg = 
     Cfg 
     { cfgPort :: !Int
       -- JSON Web Key (JWK) is a JavaScript Object Notation (JSON) 
       -- data structure that represents a cryptographic key 
     , cfgJwk :: !JWK
     , cfgIsAuthEnabled :: !Bool 
     }

run :: Cfg -> KatipContextT (ReaderT KatipEnv IO) ()
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
           ((`runReaderT` cfg) . runKatipController) 
           (toServant App.application :<|> 
            swaggerSchemaUIServerT (swaggerHttpApi cfgPort))
      let settings = 
           Warp.defaultSettings
           & Warp.setPort cfgPort
           & Warp.setOnException Warp.defaultOnException
      let runServer = 
           serveWithContext 
           (withSwagger api) 
           (defaultCookieSettings :. jwtCfg :. EmptyContext) 
           server     
      liftIO (Warp.runSettings settings runServer) `logExceptionM` ErrorS