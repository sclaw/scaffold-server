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

import Auth
import KatipController
import Data.Aeson.Unit
import Database.Transaction
import Control.Lens
import Data.Foldable

controller 
  :: Id "qualification"
  -> PatchQualification 
  -> UserId
  -> KatipController (Response Unit)
controller qualification_id PatchQualification {..} user_id  = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask 
  fmap undefined $ 
   katipTransaction hasql $ do 
    for_ patchQualificationItem $ \patch ->  
      statement Provider.patchQualification 
      (qualification_id, user_id, patch)
    for_ patchQualificationClusters $ 
      const (statement Provider.patchClusters (qualification_id, user_id))
    for_ patchQualificationTuitionFees $ 
      const (statement Provider.patchTuitionFees (qualification_id, user_id))