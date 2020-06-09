{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Auth.Password.Regenerate (controller) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Transport.Validator
import EdgeNode.Statement.Auth as Auth

import Auth
import KatipController
import Data.Aeson.Unit
import Control.Monad.IO.Class
import Data.String.Conv
import Control.Lens
import Servant.Auth.Server
import qualified Data.Text as T
import BuildInfo
import Katip
import Pretty
import Data.Traversable
import Data.Bifunctor
import Database.Transaction
import Data.Password
import Data.Functor

controller :: RegeneratePassword -> KatipController (Response Unit)
controller pass@RegeneratePassword {..} = do
  runTelegram $location pass
  jwt_sett <- fmap (^.katipEnv.jwk.to defaultJWTSettings) ask
  let err e = do
        runTelegram $location (mkPretty "error while processing reset password token" e)
        $(logTM) DebugS (logStr (mkPretty "error while processing reset password token" e))
        pure $ Error $ Error.asError @T.Text "error while processing reset password token"
  token_e <- liftIO $ getUserResetPassData jwt_sett (toS regeneratePasswordToken)
  either err (mkPassword pass) token_e

mkPassword :: RegeneratePassword -> UserResetPassData -> KatipController (Response Unit)
mkPassword x@RegeneratePassword {..} UserResetPassData {..} =
  fmap (fromValidation . first (map Error.asError)) $
  for (regeneratePassword x) $ const $ (go $> Unit)
  where
    go = do
      hasql <- fmap (^.katipEnv.hasqlDbPool) ask
      salt <- newSalt
      $(logTM) DebugS (logStr ("new password: " <> regeneratePasswordPassword))
      let hashed_password = toS $ unPassHash $ hashPassWithSalt salt (mkPass (toS regeneratePasswordPassword))
      katipTransaction hasql $ statement Auth.setNewPassword (userResetPassDataUserId, userResetPassDataTokenType, hashed_password)