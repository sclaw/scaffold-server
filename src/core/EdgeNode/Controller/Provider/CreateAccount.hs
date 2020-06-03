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

import Auth
import KatipController
import Katip
import Database.Transaction
import Control.Lens
import Pretty
import BuildInfo
import Data.Aeson.Unit
import Data.Traversable
import Data.Bifunctor
import Data.Foldable
import Validation
import Data.Functor
import Control.Monad.IO.Class
import Data.String.Conv
import System.Random
import Data.Elocrypt
import Data.Coerce

controller :: NewAccount -> UserId -> KatipController (Response Unit)
controller account user_id = do
  $(logTM) DebugS (logStr ("new account: " ++ mkPretty mempty (account, user_id)))
  runTelegram $location (account, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  telegram <- fmap (^.katipEnv.telegram) ask
  response <- fmap (fromValidation . (bimap (map asError) (const Unit)) . sequenceA_) $
    for (newAccount account) $ \(email, role) ->
    fmap (eitherToValidation . first (const [NewAccountErrorTaken])) $
    katipTransactionViolationError hasql $ do
      (password, _) <- liftIO $ fmap (genPassword 16 (GenOptions True True True)) newStdGen
      ident <- statement Provider.createAccount (coerce user_id, email, toS password)
      log <- ask
      liftIO $ send telegram log $ toS $ "At module " <> $location <> "new account: " <> show (ident, password)
      statement Rbac.assignRoleToUser (ident, role, Nothing)
  runTelegram $location response $> response