{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Auth.RefreshAccessToken (controller) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response hiding (Error)
import qualified EdgeNode.Transport.Error as Error
import qualified EdgeNode.Statement.Auth as Auth
import EdgeNode.Transport.Id
import EdgeNode.Controller.Auth.SignIn ()

import TH.Proto
import Katip
import Auth
import KatipController
import Data.Generics.Product.Fields
import Control.Lens
import Control.Monad.Except
import Servant.Auth.Server
import qualified Data.Text as T
import Data.Bifunctor
import Data.Traversable
import qualified Crypto.JWT as JWT
import Database.Transaction
import Data.Aeson
import Control.Lens.Iso.Extended
import Data.Foldable
import Pretty
import BuildInfo
import Data.Functor

controller :: Token -> Id "user" -> KatipController (Response Tokens)
controller req user_id = do
  let token = req^.field @"tokenToken"
  $(logTM) DebugS (logStr (mkPretty "refresh token request:" token))
  key <- fmap (^.katipEnv.jwk) ask
  runTelegram $location (req, user_id)

  verify_result <- liftIO $ runExceptT $ verifyToken (defaultJWTSettings key) token
  for_ (verify_result^?_Left) $ \e -> $(logTM) ErrorS (logStr (mkPretty "refresh token error:" e))
  response <- fmap (fromEither . first (Error.asError @T.Text) . join) $
    for (first mkJWTError verify_result) $ \claims -> do
      hasql <- fmap (^.katipEnv.hasqlDbPool) ask
      key <- fmap (^.katipEnv.jwk) ask
      TokensLT {..} <- fmap (^.katipEnv.tokensLT) ask
      let uqm = claims^.JWT.unregisteredClaims.at "dat".to (fmap fromJSON)
      case uqm of
        Just (Success uq) ->
          katipTransaction hasql $ do
            tpl_m <- statement Auth.checkRefreshToken (uq, user_id)
            case tpl_m of
              Just (ident, email, user_role) -> do
                log <- ask
                tokens_e <- liftIO $
                  Auth.mkTokens
                  key
                  (user_id, email, user_role)
                  log
                  tokensLTAccessLT
                  tokensLTRefreshLT
                void $ statement Auth.mkTokenInvalid ident
                for tokens_e $ \(uq, a, r, h) -> do
                  void $ statement Auth.putRefreshToken (user_id, h, uq)
                  pure $
                    def & field @"tokensAccessToken" .~ (a^._1)
                        & field @"tokensRefreshToken" .~ r
                        & field @"tokensLifetime" ?~ (a^._2)
              Nothing -> pure $ Left "token not found"
        Nothing -> pure $ Left "dat not found"
  runTelegram $location (token, user_id) $> response

mkJWTError :: JWT.JWTError -> T.Text
mkJWTError JWT.JWTExpired = ErrorRefreshTokenExpired^.isoError.stext
mkJWTError _ = ErrorJWTError^.isoError.stext