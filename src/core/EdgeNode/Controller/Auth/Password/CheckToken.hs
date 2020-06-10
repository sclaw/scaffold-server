{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.Auth.Password.CheckToken (controller) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response

import Auth
import KatipController
import Servant.Auth.Server
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.String.Conv
import BuildInfo

controller :: TokenCheck -> KatipController (Response Bool)
controller token = do
  jwt_sett <- fmap (^.katipEnv.jwk.to defaultJWTSettings) ask
  verified_token_e <- liftIO $ runExceptT $ verifyToken jwt_sett (toS (tokenCheckToken token))
  runTelegram $location verified_token_e
  pure $ either (const (Ok False)) (const (Ok True)) verified_token_e