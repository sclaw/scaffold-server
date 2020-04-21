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

controller
  :: Id "qualification"
  -> PatchQualification
  -> UserId
  -> KatipController (Response Unit)
controller qualification_id patch@(PatchQualification {..}) user_id  = do
  $(logTM) DebugS (logStr ("qualification to be patched: " ++ mkPretty mempty patch))
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  fmap (fromValidation . bimap (map Error.asError) (const Unit)) $
    for (qualififcationPatch patch) $ const $
      katipTransaction hasql $ do
        for_ patchQualificationItem $ \patch ->
          statement Provider.patchQualification
         (qualification_id, user_id, patch)
        -- cluster block
        for_ patchQualificationClusters $
          const (statement Provider.patchClusters qualification_id)
        for_ patchQualificationDeletionClusters $
          const (statement Provider.deleteClusters qualification_id)
        for_ patchQualificationDeletionDeps $
          const (statement Provider.deleteDeps qualification_id)
        -- fees block
        for_ patchQualificationDeletionFees $ \(PatchFeesDeletion xs) ->
          statement Provider.deleteFees (qualification_id, xs)
        for_ patchQualificationTuitionFees $ \(PatchTuitionFees xs) ->
          statement Provider.patchTuitionFees (qualification_id, xs)