{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Provider.GetBranches (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Extended (GetBranchResp)

import Katip
import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Data.Functor

controller
  :: Id "user"
  -> KatipController
     (Response [GetBranchResp])
controller uid = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  runTelegram uid
  response <- katipTransaction hasql (statement Provider.getBranches uid)
  $(logTM) DebugS (logStr ("branches: " ++ mkPretty mempty response))
  runTelegram response $> Ok response