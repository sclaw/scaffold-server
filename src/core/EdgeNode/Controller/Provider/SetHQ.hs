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

controller :: Id -> Id -> KatipController (Response Unit)
controller branchId userId = fmap (^.katipEnv.hasqlDbPool) ask >>= (`katipTransaction` (fmap mkResp (statement Provider.setHQ (userId, branchId))))
  where mkResp True = Ok Unit
        mkResp False = Error $ Error.asError @T.Text "not found"