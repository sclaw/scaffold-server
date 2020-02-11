{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EdgeNode.Controller.Provider.CreateBranches (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id
import qualified EdgeNode.Statement.Provider as Provider

import KatipController
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens
import Data.Maybe

controller :: [OptField "files" [Id] (OptField "image" Id Branch)] -> Id -> KatipController (Response [Id])
controller xs uid = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  result <- katipTransactionE hasql $ do
    let (files, branches) = 
          unzip $ 
          flip map xs $ 
          \(OptField (WithField fs x)) -> 
           (fromMaybe [] fs, x) 
    ids <- statement Provider.createBranches (uid, branches)
    statement Provider.createFiles $ 
      concat (zipWith (\i xs -> zip (repeat i) xs) ids files)
    return ids
  return $ fromEither result  