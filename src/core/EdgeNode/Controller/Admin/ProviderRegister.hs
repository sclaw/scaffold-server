{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module EdgeNode.Controller.Admin.ProviderRegister (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Transport.Provider
import EdgeNode.Statement.Admin as Admin
import EdgeNode.Model.User
import EdgeNode.Statement.Rbac as Rbac
import EdgeNode.Model.Rbac
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Transport.Iso

import Auth
import Katip
import KatipController
import Control.Lens
import Data.Generics.Product.Fields
import Control.Lens.Iso.Extended
import System.Random
import Control.Monad.IO.Class
import Data.Elocrypt
import Database.Transaction
import Data.Coerce
import Data.Password
import qualified Data.Text as T
import Data.Traversable
import Data.Functor

data ProviderRegistrationError = ProviderAlreadyExist

deriving instance Enum ProviderCategory

instance Error.AsError ProviderRegistrationError where
  asError ProviderAlreadyExist = Error.asError @T.Text "provider already registered"

controller :: ProviderRegistration -> BasicUser -> KatipController (Response T.Text)
controller provider user = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  (password, _) <- liftIO $ fmap (genPassword 16 (GenOptions True True True)) newStdGen
  $(logTM) DebugS (logStr ("admin password: " <> password))
  salt <- newSalt
  let hashedPassword = hashPassWithSalt salt (mkPass (password^.stext))
  let providerExt =
        ProviderRegistrationExt
        (provider^.field @"providerRegistrationAdminEmail")
        (provider^.field @"providerRegistrationProviderUID")
        (provider^.field @"providerRegistrationTitle")
        (provider^.field @"providerRegistrationCategory".providerCategory.from lazytext)
        (unPassHash hashedPassword^.from lazytext)
        (Secondary^.isoUserRole.stextl)
        (Active^.isoRegisterStatus.stextl)

  runTelegram providerExt

  response <- fmap (maybe (Error (Error.asError ProviderAlreadyExist)) (const (Ok (password^.stext)))) $
    katipTransaction hasql $ do
      ident <- statement Admin.newProvider providerExt
      for ident $ \x -> statement Rbac.assignRoleToUser (x, RoleProvider, Just (coerce (basicUserUserId user)))
  runTelegram response $> response