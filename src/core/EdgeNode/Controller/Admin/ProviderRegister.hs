{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Admin.ProviderRegister (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Transport.Provider
import EdgeNode.Statement.Admin as Admin
import EdgeNode.Model.User
import EdgeNode.Statement.Rbac as Rbac
import EdgeNode.Model.Rbac

import Auth
import Katip
import KatipController
import Data.Aeson.Unit
import Control.Lens
import Data.Generics.Product.Fields
import Control.Lens.Iso.Extended
import System.Random
import Control.Monad.IO.Class
import Data.Elocrypt
import Database.Transaction
import Data.Foldable
import Data.Coerce
import Data.Password

controller :: ProviderRegistration -> BasicUser -> KatipController (Response Unit)
controller provider user = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  (password, _) <- liftIO $ fmap (genPassword 8 (GenOptions True True True)) newStdGen
  $(logTM) DebugS (logStr ("admin password: " <> password))
  salt <- newSalt
  let hashedPassword = hashPassWithSalt salt (mkPass (password^.stext)) 
  let providerExt = 
        ProviderRegistrationExt 
        (provider^.field @"providerRegistrationProviderUID")
        (provider^.field @"providerRegistrationAdminEmail")
        (unPassHash hashedPassword^.from lazytext)
        (Secondary^.isoType.stextl)
        (Active^.isoRegisterStatus.stextl)
  fmap (const (Ok Unit)) $ katipTransaction hasql $ do 
    ident <- statement Admin.newProvider providerExt
    for_ ident $ \x -> statement Rbac.assignRoleToUser (x, RoleProvider, coerce (basicUserUserId user))