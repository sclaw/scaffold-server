{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module EdgeNode.Controller.Auth.Password.New (controller, mkToken) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Statement.Auth as Auth
import EdgeNode.Statement.Mail as Mail
import EdgeNode.Mail
import qualified EdgeNode.Model.Auth as Auth

import Auth
import KatipController
import Data.Aeson.Unit
import BuildInfo
import Control.Lens
import Control.Monad.IO.Class
import Database.Transaction
import qualified Data.Text as T
import Data.Functor
import Data.Aeson (toJSON)
import qualified Protobuf.Scalar as Protobuf
import Data.String.Conv
import qualified Data.ByteString as B
import Data.Time.Clock
import Data.Maybe
import Data.Bifunctor
import Data.Traversable
import Data.Foldable

controller :: JWTUser -> KatipController (Response Unit)
controller user | T.null (jWTUserEmail user) =
  pure $ Error $ Error.asError @T.Text "email empty. please, notify developers about this issue"
controller user@JWTUser {..} = do
  key <- fmap (^.katipEnv.jwk) ask
  runTelegram $location user
  logger <- askLoggerIO
  let token_dat =
        UserResetPassData
        jWTUserUserId
        jWTUserUserRole
        jWTUserEmail
        Auth.NewPassword
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  Urls {..} <- fmap (^.katipEnv.urls) ask
  token_e <- liftIO $ mkPasswordResetToken key token_dat logger
  fmap (fromEither . second (const Unit) . sequenceA_) $ for token_e $
      katipTransaction hasql
    . mkToken urlsResetPassword jWTUserUserId jWTUserEmail Auth.NewPassword

mkToken :: T.Text -> UserId -> T.Text -> Auth.TokenType -> (B.ByteString, UTCTime) -> SessionR (Either T.Text ())
mkToken urlsResetPassword user_id email token_type (token, valid_until) = do
  token_status_m <- statement getTokenStatus (user_id, token_type)
  curr_tm <- liftIO getCurrentTime
  case token_status_m of
    Nothing -> putToken
    Just tm
      | fmap (curr_tm <) tm == Just True ->
        pure $ Left $ "block until " <> toS (show (fromJust tm))
      | otherwise -> putToken
  where
    putToken = do
      name <-
        statement
        Auth.putResetPasswordToken
        (user_id, token_type, token, valid_until)
      let mail_data =
           ( email
           , TypeResetPassword
           , StatusNew
           , toJSON (
              ResetPassword
              (fmap (Protobuf.String . toS) name)
              (toS urlsResetPassword <> toS token)))
      statement Mail.new mail_data $> Right ()