{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Provider.CreateAccount (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider.Settings
import EdgeNode.Transport.Validator
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Statement.Rbac as Rbac
import EdgeNode.Statement.Mail as Mail
import EdgeNode.Mail

import Auth
import KatipController
import Katip
import Database.Transaction
import Control.Lens
import Pretty
import BuildInfo
import Data.Aeson.Unit
import Data.Traversable
import Validation
import Control.Monad.IO.Class
import Data.String.Conv
import System.Random
import Data.Elocrypt
import Data.Coerce
import Data.Password
import Control.Lens.Iso.Extended
import Data.Functor
import Data.Bifunctor
import Data.Aeson (toJSON)

controller :: NewAccount -> UserId -> KatipController (Response Unit)
controller account user_id = do
  $(logTM) DebugS (logStr ("new account: " ++ mkPretty mempty (account, user_id)))
  runTelegram $location (account, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  telegram <- fmap (^.katipEnv.telegram) ask
  response <- for (newAccount account) $ \(email, role) ->
    fmap (eitherToValidation . first (const [NewAccountErrorTaken])) $
    katipTransactionViolationError hasql $ do
      (password, _) <- liftIO $ fmap (genPassword 16 (GenOptions True True True)) newStdGen
      salt <- liftIO newSalt
      let hashedPassword = toS $ unPassHash $ hashPassWithSalt salt (mkPass (password^.stext))
      (ident, provider_id) <- statement Provider.createAccount (coerce user_id, email, hashedPassword)
      statement Mail.new
       ( email
       , TypeNewProviderAccount
       , StatusNew
       , toJSON (
          NewProviderAccount
          (toS email)
          (toS provider_id)
          (toS password)
          (toS email)))
      log <- ask
      liftIO $ send telegram log $ toS $ "At module " <> $location <> " new account: " <> show (ident, password)
      statement Rbac.assignRoleToUser (ident, role, Nothing)
  let mk (Success (Failure e)) = Failure $ map asError e
      mk (Success (Success _)) = Success Unit
      mk (Failure e) = Failure $ map asError e
  runTelegram $location response $> (fromValidation . mk) response
