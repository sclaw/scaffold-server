{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Auth.Registration (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Auth
import qualified EdgeNode.Statement.Auth as Auth
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Statement.Rbac as Rbac
import EdgeNode.Model.Rbac
import EdgeNode.Transport.Validator

import Katip
import KatipController
import Data.Aeson.Unit
import qualified Data.Text as T
import Database.Transaction
import Control.Lens
import Data.Password
import Pretty
import Data.Traversable
import Data.Bifunctor
import Control.Concurrent.Lifted
import Control.Monad.IO.Class
import Control.Monad

controller :: Registration -> KatipController (Response Unit)
controller registeration_data = do
  $(logTM) DebugS (logStr (mkPretty "registeration data:" registeration_data))
  telegram_service <- fmap (^.katipEnv.telegram) ask
  logger <- askLoggerIO
  void $ fork $ liftIO $ send telegram_service logger (T.pack (show registeration_data))
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  resp <- fmap (fromValidation . first (map asError)) $
    for (registration registeration_data) $ const $ do
      salt <-  newSalt
      katipTransaction hasql $ do
        ident_m <- statement (Auth.register salt) registeration_data
        for ident_m $ \x ->
          statement
          Rbac.assignRoleToUser
          (x, RoleUser, Nothing)
  return $ case resp of
    Ok (Just _) -> Ok Unit
    Ok Nothing -> Error (Error.asError @T.Text "email taken")
    Error e  -> Error e
    Errors es  -> Errors es