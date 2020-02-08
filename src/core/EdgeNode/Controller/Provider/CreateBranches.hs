{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.CreateBranches (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id
import qualified EdgeNode.Statement.Provider as Provider 

import KatipController
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens

controller :: [OptField "files" [Id] (OptField "image" Id Branch)] -> Id -> KatipController (Response [Id])
controller branches uid = fmap (^.katipEnv.hasqlDbPool) ask >>= (`katipTransaction` (fmap Ok (statement Provider.createBranches (uid, branches))))