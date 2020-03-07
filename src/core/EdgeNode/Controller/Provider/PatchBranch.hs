{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Provider.PatchBranch (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import qualified EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Extended

import Katip
import KatipController
import Data.Aeson.Unit
import Pretty
import Data.Generics.Product.Fields
import Control.Lens
import Database.Transaction

controller :: [PatchBranchReq] -> Id "user" -> KatipController (Response Unit)
controller xs _ = do
  $(logTM) DebugS (logStr ("branches to be patched: " ++ mkPretty mempty xs))
  let xs' = xs <&> \x -> 
        ( x^.field @"patchBranchReqId"
        , x^.field @"patchBranchReqImg"
        , x^.field @"patchBranchReqBranch"
        )
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (const (Ok Unit)) $ 
    katipTransactionViolationError hasql $ 
    statement Provider.updateBranches xs' 