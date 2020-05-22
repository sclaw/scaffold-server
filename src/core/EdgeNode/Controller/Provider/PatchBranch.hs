{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Provider.PatchBranch (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Extended

import Auth
import Katip
import KatipController
import Data.Aeson.Unit
import Pretty
import Data.Generics.Product.Fields
import Control.Lens
import Database.Transaction
import BuildInfo

controller :: [PatchBranchReq] -> UserId -> KatipController (Response Unit)
controller xs user_id = do
  $(logTM) DebugS (logStr ("branches to be patched: " ++ mkPretty mempty xs))
  runTelegram $location (xs, user_id)
  let xs' = xs <&> \x ->
        ( x^.field @"patchBranchReqId"
        , x^.field @"patchBranchReqImg"
        , x^.field @"patchBranchReqBranch"
        )
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (const (Ok Unit)) $
    katipTransactionViolationError hasql $ do
      statement Provider.apiCaller ($location, user_id)
      statement Provider.updateBranches xs'