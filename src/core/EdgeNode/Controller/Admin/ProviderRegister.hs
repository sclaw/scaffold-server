{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Admin.ProviderRegister (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider
import EdgeNode.Statement.Admin as Admin
import EdgeNode.Model.User

import KatipController
import Data.Aeson.Unit
import Control.Lens
import Data.Generics.Product.Fields
import Control.Lens.Iso.Extended
import System.Random
import Control.Monad.IO.Class
import Data.Elocrypt
import Database.Transaction

controller :: ProviderRegistration -> KatipController (Response Unit)
controller provider = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  (password, _) <- liftIO $ fmap (genPassword 8 (GenOptions True True True)) newStdGen 
  let providerExt = 
        ProviderRegistrationExt 
        (provider^.field @"providerRegistrationAdminEmail")
        (provider^.field @"providerRegistrationProviderUID")
        (password^.stextl)
        (Secondary^.isoType.stextl)
        (Active^.isoRegisterStatus.stextl)
  fmap (const (Ok Unit)) $ katipTransaction hasql $ statement Admin.newProvider providerExt