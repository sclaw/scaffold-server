{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
import BuildInfo
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Protobuf.Scalar as Protobuf

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
          let xs_cls = mkClusters $ patchClustersPatched x
          statement Provider.patchClusters xs_cls
          let xs_deps = mkDependencies $ patchClustersPatched x
          statement Provider.patchDeps (qualification_id, xs_deps)
        for_ patchQualificationDeletionClusters $
          const (statement Provider.deleteClusters qualification_id)
        for_ patchQualificationDeletionDeps $
          const (statement Provider.deleteDeps qualification_id)
        -- fees block
        for_ patchQualificationDeletionFees $ \(PatchFeesDeletion xs) ->
          statement Provider.deleteFees (qualification_id, xs)
        for_ patchQualificationTuitionFees $ \(PatchTuitionFees xs) ->
          statement Provider.patchTuitionFees (qualification_id, xs)

mkDependencies :: V.Vector PatchClusters_Patch -> V.Vector (Int64, Dependency)
mkDependencies = V.concat . foldr mk mempty
  where mk (PatchClusters_Patch ident (Just (Cluster _ v))) xs = V.map (fromIntegral ident,) v : xs
        mk (PatchClusters_Patch _ Nothing) xs = xs

mkClusters :: V.Vector PatchClusters_Patch -> V.Vector (Int64, Maybe T.Text)
mkClusters v = v <&> \(PatchClusters_Patch ident (Just (Cluster title _))) -> (fromIntegral ident, fmap (LT.toStrict . Protobuf.stringValue) title)