{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Auth.Registration (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Auth
import qualified EdgeNode.Statement.Auth as Auth
import qualified EdgeNode.Transport.Error as Error

import KatipController
import Data.Aeson.Unit
import Data.Aeson.WithField ()
import qualified Data.Text as T
import Database.Transaction
import Data.Bifunctor ()
import Control.Lens
import Data.Password

controller :: Registration -> KatipController (Response Unit)
controller registeration_data | 
  registrationPassword registeration_data /= 
  registrationPasswordOnceAgain registeration_data
  = return $ Error $ Error.asError @T.Text "passwords mismatch"
controller registeration_data = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let mkResp x 
        | not x = Error (Error.asError @T.Text "email taken")
        | otherwise = Ok Unit
  salt <-  newSalt
  fmap mkResp $ katipTransaction hasql $ statement (Auth.register salt) registeration_data