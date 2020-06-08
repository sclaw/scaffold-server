{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.Auth.Password.New (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Statement.Auth as Auth
import EdgeNode.Statement.Mail as Mail
import EdgeNode.Mail

import Auth
import KatipController
import Data.Aeson.Unit
import BuildInfo
import Control.Lens
import Control.Monad.IO.Class
import Database.Transaction
import qualified Data.Text as T
import Data.Functor
import Data.Traversable
import Data.Aeson (toJSON)
import qualified Protobuf.Scalar as Protobuf
import Data.String.Conv

controller :: JWTUser -> KatipController (Response Unit)
controller user@JWTUser {..} = do
  key <- fmap (^.katipEnv.jwk) ask
  runTelegram $location user
  logger <- askLoggerIO
  let token_dat = UserResetPassData jWTUserUserId jWTUserUserRole jWTUserEmail
  token_e <- liftIO $ mkPasswordResetToken key token_dat logger
  case token_e of
    Left e -> pure $ Error (Error.asError e)
    Right token -> do
      hasql <- fmap (^.katipEnv.hasqlDbPool) ask
      Urls {..} <- fmap (^.katipEnv.urls) ask
      fmap (flip (liftMaybe @T.Text) "attempts out") $
        katipTransaction hasql $ do
          res_m <- statement Auth.putResetPasswordToken (jWTUserUserId, token)
          for res_m $ \name -> do
            let mail_data =
                  ( jWTUserEmail
                  , TypeResetPassword
                  , StatusNew
                  , toJSON (
                     ResetPassword
                     (fmap (Protobuf.String . toS) name)
                     (toS urlsResetPassword <> toS token))
                  )
            statement Mail.new mail_data $> Unit