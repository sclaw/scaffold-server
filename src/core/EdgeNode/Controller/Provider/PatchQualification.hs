{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeNode.Controller.Provider.PatchQualification (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Transport.Qualification
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Validator
import qualified EdgeNode.Transport.Error as Error

import Auth
import Katip
import Pretty
import KatipController
import Data.Aeson.Unit
import Database.Transaction
import Control.Lens
import Data.Foldable
import Data.Traversable
import Data.Int
import qualified Data.Vector as V
import qualified Data.Map as Map
import BuildInfo

controller
  :: Id "qualification"
  -> PatchQualification
  -> UserId
  -> KatipController (Response Unit)
controller qualification_id patch@(PatchQualification {..}) user_id  = do
  $(logTM) DebugS (logStr ("qualification to be patched: " ++ mkPretty mempty patch))
  runTelegram $location (qualification_id, patch, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (fromValidation . bimap (map Error.asError) (const Unit)) $
    for (qualififcationPatch patch) $ const $
      katipTransaction hasql $ do
        statement Provider.apiCaller ($location, user_id)
        for_ patchQualificationItem $ \patch ->
          statement Provider.patchQualification
         (qualification_id, user_id, patch)
        -- cluster block
        for_ patchQualificationClusters $ \x -> do
          let xs = mkDependencies $ patchClustersClusters x
          statement Provider.patchClusters (qualification_id, xs)
        for_ patchQualificationDeletionClusters $
          const (statement Provider.deleteClusters qualification_id)
        for_ patchQualificationDeletionDeps $
          const (statement Provider.deleteDeps qualification_id)
        -- fees block
        for_ patchQualificationDeletionFees $ \(PatchFeesDeletion xs) ->
          statement Provider.deleteFees (qualification_id, xs)
        for_ patchQualificationTuitionFees $ \(PatchTuitionFees xs) ->
          statement Provider.patchTuitionFees (qualification_id, xs)

mkDependencies :: V.Vector Cluster -> V.Vector (Int32, Int32, Dependency)
mkDependencies = V.fromList . Map.elems . ifoldr goCluster mempty
  where
    goCluster i (Cluster _ deps) v = ifoldr (goDeps i) v deps
    goDeps c p  x = Map.insert (c, dependencyDependency x)  (fromIntegral c, fromIntegral p, x)