{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Auth.Password.Reset (controller) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response
import EdgeNode.Statement.Auth as Auth
import qualified EdgeNode.Transport.Error as Error
import qualified EdgeNode.Model.Auth as Auth
import EdgeNode.Controller.Auth.Password.New (mkToken)
import EdgeNode.Model.User

import Auth
import KatipController
import Data.Aeson.Unit
import Control.Lens
import Database.Transaction
import Validation
import Data.Traversable
import Data.Foldable
import Data.String.Conv
import Data.Bifunctor
import qualified Data.Text as T
import BuildInfo
import Control.Monad.IO.Class

controller :: ResetPassword -> KatipController (Response Unit)
controller reset@ResetPassword {..} = do
  runTelegram $location reset
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  Urls {..} <- fmap (^.katipEnv.urls) ask
  key <- fmap (^.katipEnv.jwk) ask
  logger <- askLoggerIO
  response_m <- katipTransaction hasql $ do
    user_id_m <- statement Auth.getUserId (Primary, toS resetPasswordEmail)
    for user_id_m $ \user_id -> do
      let token_dat = UserResetPassData user_id Primary (toS resetPasswordEmail) Auth.ForgotPassword
      token_e <- liftIO $ mkPasswordResetToken key token_dat logger
      fmap (sequenceA_ . first (Failure . (:[]) . Error.asError)) $ for token_e $
          fmap (eitherToValidation . first ((:[]) . Error.asError))
        . mkToken urlsResetPassword user_id (toS resetPasswordEmail) Auth.ForgotPassword
  runTelegram $location response_m
  pure $ case response_m of
    Nothing -> Error $ Error.asError @T.Text "email not found"
    Just response -> (fromValidation . second (const Unit)) response