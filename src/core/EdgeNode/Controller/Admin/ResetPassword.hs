{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Admin.ResetPassword (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Transport.Error as Error
import EdgeNode.Statement.Admin as Admin

import Katip
import KatipController
import Control.Lens
import Control.Lens.Iso.Extended
import System.Random
import Control.Monad.IO.Class
import Data.Elocrypt
import Database.Transaction
import Data.Password
import qualified Data.Text as T
import Data.Aeson.WithField
import Data.Coerce

controller :: T.Text -> OnlyField "email" T.Text -> KatipController (Response T.Text)
controller provider_id email = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  (password, _) <- liftIO $ fmap (genPassword 16 (GenOptions True True True)) newStdGen
  $(logTM) DebugS (logStr ("new admin password: " <> password))
  runTelegram (provider_id, email, password)
  salt <- newSalt
  let mkResp 1 = Ok (password^.stext)
      mkResp _ = Error $ Error.asError User404
  fmap mkResp $
    katipTransaction hasql $
    statement Admin.resetPassword
    (provider_id, coerce email, unPassHash (hashPassWithSalt salt (mkPass (password^.stext))))

data ResetError = User404

instance Error.AsError ResetError where
  asError User404 = Error.asError @T.Text "user not found"