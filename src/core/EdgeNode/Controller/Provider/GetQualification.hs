{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Provider.GetQualification (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Provider.Qualification

import Auth
import Katip
import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Data.Aeson.WithField
import Data.Bifunctor
import BuildInfo

controller
  :: Id "qualification"
  -> UserId
  -> KatipController
     (Response
      (WithId (Id "qualification")
       QualificationInfo))
controller qualififcation_id user_id = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  runTelegram $location (qualififcation_id, user_id)
  response <- katipTransaction hasql $
    statement
    Provider.getQualificationById
    (user_id, qualififcation_id)
  $(logTM) DebugS $ logStr $
    "qualification: " ++
    mkPretty mempty response
  runTelegram $location response
  return $ fromEither $ (second (WithField qualififcation_id)) response