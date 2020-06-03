{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Provider.CreateAccount (controller) where

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

controller :: NewAccount -> UserId -> KatipController (Response Unit)
controller account user_id = do
  $(logTM) DebugS (logStr ("new account: " ++ mkPretty mempty (account, user_id)))
  runTelegram $location (account, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- fmap (fromValidation . (bimap (map asError) (const Unit)) . sequenceA_) $
    for (newAccount account) $ \(email, role) ->
    fmap (eitherToValidation . first (const [NewAccountErrorTaken])) $
    katipTransactionViolationError hasql $ do
      ident <- statement Provider.createAccount email
      statement Rbac.assignRoleToUser (ident, role, Nothing)
  runTelegram $location response $> response