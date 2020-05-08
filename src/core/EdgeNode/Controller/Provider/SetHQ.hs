{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.SetHQ (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import qualified EdgeNode.Statement.Provider as Provider
import qualified EdgeNode.Transport.Error as Error

import KatipController
import Data.Aeson.Unit
import Control.Lens
import Database.Transaction
import qualified Data.Text as T

controller :: Id "branch"  -> Id "user" -> KatipController (Response Unit)
controller branch_id user_id = do
  runTelegram (branch_id, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  let mkResp True = Ok Unit
      mkResp False = Error $ Error.asError @T.Text "not found"
  katipTransaction hasql $ fmap mkResp $
    statement Provider.setHQ (user_id, branch_id)