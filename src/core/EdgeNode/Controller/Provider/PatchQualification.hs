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
import EdgeNode.Transport.Provider.Qualification
import qualified EdgeNode.Statement.Provider as Provider
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
import qualified Data.Vector as V
import BuildInfo
import qualified Data.Text.Lazy as LT
import qualified Protobuf.Scalar as Protobuf
import Data.Word
import Control.Monad.IO.Class
import Data.String.Conv

controller
  :: Id "qualification"
  -> PatchQualification
  -> UserId
  -> KatipController (Response Unit)
controller qualification_id patch@(PatchQualification {..}) user_id  = do
  $(logTM) DebugS (logStr ("qualification to be patched: " ++ mkPretty mempty patch))
  runTelegram $location (qualification_id, patch, user_id)
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  tel_service <- fmap (^.katipEnv.telegram) ask
  fmap (fromValidation . bimap (map Error.asError) (const Unit)) $
    for (qualififcationPatch patch) $ const $
      katipTransaction hasql $ do
        statement Provider.apiCaller ($location, user_id)
        for_ patchQualificationItem $ \patch ->
          statement Provider.patchQualification
         (qualification_id, user_id, patch)
        -- <<begin: cluster block
        for_ patchQualificationClusters $ \x -> do
          addClusters tel_service qualification_id (patchClustersAdded x)
          patchClusters tel_service qualification_id (patchClustersPatched x)
          deleteClusters tel_service qualification_id (patchClustersDeleted x)
        for_ patchQualificationDeletionClusters $
          const (statement Provider.deleteClusters qualification_id)
        for_ patchQualificationDeletionDeps $
          const (statement Provider.deleteDeps qualification_id)
        --  <<end: cluster block
        -- <<begin: fees block
        for_ patchQualificationDeletionFees $ \(PatchFeesDeletion xs) ->
          statement Provider.deleteFees (qualification_id, xs)
        for_ patchQualificationTuitionFees $ \(PatchTuitionFees xs) ->
          statement Provider.patchTuitionFees (qualification_id, xs)
        -- <<end: fees block

addClusters :: Service -> Id "qualification" -> V.Vector PatchClusters_New -> SessionR ()
addClusters telegram ident clusters = do
  log <- ask
  let debug = mkPretty ("At module " <> $location) ("patch qualification, add new clusters: " <> show clusters)
  liftIO $ send telegram log $ toS debug
  let mkCluster (pos, cl) =
        ( fromIntegral pos
        , fmap (LT.toStrict . Protobuf.stringValue)
          (patchClusters_NewTitle cl))
  ids <- statement Provider.saveClusters (ident, V.indexed clusters <&> mkCluster)
  statement Provider.saveDependencies (ident, V.zipWith (\i cl -> (i, patchClusters_NewDependencies cl)) ids clusters)

patchClusters :: Service -> Id "qualification" -> V.Vector PatchClusters_Patch -> SessionR ()
patchClusters telegram ident clusters = do
  log <- ask
  let debug = mkPretty ("At module " <> $location) ("patch qualification, patch clusters: " <> show clusters)
  liftIO $ send telegram log $ toS debug
  let xs_cls =
        clusters <&> \(PatchClusters_Patch ident title _) ->
          (fromIntegral ident, fmap (LT.toStrict . Protobuf.stringValue) title)
  statement Provider.patchClusters xs_cls
  let mk (PatchClusters_Patch ident _ v) xs = V.map (fromIntegral ident,) v : xs
  let xs_deps = V.concat $ foldr mk mempty clusters
  statement Provider.patchDeps (ident, xs_deps)

deleteClusters :: Service -> Id "qualification"  -> V.Vector Word64 -> SessionR ()
deleteClusters telegram _ _ = do
  log <- ask
  let debug = mkPretty ("At module " <> $location) ("patch qualification, delete clusters, not implemented yet")
  liftIO $ send telegram log $ toS debug