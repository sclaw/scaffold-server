{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.GetBranches (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Extended (GetBranchResp)

import KatipController
import Database.Transaction
import Control.Lens

controller 
  :: Id "user" 
  -> KatipController 
     (Response [GetBranchResp])
controller uid = 
  fmap (^.katipEnv.hasqlDbPool) ask >>= 
  (`katipTransaction` (fmap Ok (statement Provider.getBranches uid)))