{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Provider.CreateBranches (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Extended
import EdgeNode.Transport.Id
import qualified EdgeNode.Statement.Provider as Provider

import Auth
import Katip
import KatipController
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens
import Data.Maybe
import Pretty
import BuildInfo

controller :: [MkBranchReq] -> UserId -> KatipController (Response [Id "branch"])
controller xs user_id = do
  $(logTM) DebugS (logStr ("branches: " ++ mkPretty mempty xs))
  runTelegram $location (xs, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- katipTransactionViolationError hasql $ do
    statement Provider.apiCaller ($location, user_id)
    let (files, branches) =
          unzip $
          flip map xs $
          \(MkBranchReq fs img x) ->
           (fromMaybe [] fs, OptField (WithField img x))
    ids <- statement Provider.createBranches (user_id, branches)
    statement Provider.createFiles $
      concat (zipWith (\i xs -> zip (repeat i) xs) ids files)
    return ids
  runTelegram $location response
  return $ fromEither response