{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdgeNode.Controller.Auth.SignIn (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.Auth
import qualified EdgeNode.Statement.Auth as Auth
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Model.User

import Katip.Monadic
import KatipController
import Control.Lens
import Database.Transaction
import Data.Traversable
import Data.Maybe
import qualified Data.Text as T
import Data.Password
import Data.Generics.Product.Fields
import Control.Lens.Iso.Extended
import Control.Monad.IO.Class
import qualified Auth as Auth
import Control.Monad.Except
import Data.Bifunctor
import Data.Aeson.WithField
import Data.Tuple.Ops
import Data.Default.Class
import Control.Concurrent.Lifted

instance Default Tokens

controller :: Signin -> KatipController (Response (WithId (Id "user") (WithField "role" UserRole Tokens)))
controller req = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  cred_m <- katipTransaction hasql $ statement Auth.getUserCred req
  telegram_service <- fmap (^.katipEnv.telegram) ask
  logger <- askLoggerIO
  void $ fork $ liftIO $ send telegram_service logger (T.pack (show (req & field @"signinPassword" .~ "****")))
  fmap (fromMaybe (Error (Error.asError @T.Text "credential not found"))) $
    for cred_m $ \ cred -> do
      let check = checkPass (req^.field @"signinPassword".lazytext.to mkPass) (cred^._3)
      case check of
        PassCheckSuccess -> fmap (fromEither . first (Error.asError @T.Text)) $ do
          key <- fmap (^.katipEnv.jwk) ask
          log <- askLoggerIO
          tokens_e <- liftIO $ Auth.mkTokens key (initT cred) log
          for tokens_e $ \tpl@(uq, a, r, h) -> do
            hasql <- fmap (^.katipEnv.hasqlDbPool) ask
            void $ katipTransaction hasql $
              statement Auth.putRefreshToken
              (cred^._1, h, uq)
            pure $ WithField (cred^._1) $
                   WithField (cred^._2) $
                   def & field @"tokensAccessToken" .~ (a^._1)
                       & field @"tokensRefreshToken" .~ r
                       & field @"tokensLifetime" ?~ (a^._2)
        PassCheckFail -> pure $ Error (Error.asError @T.Text "wrong password")