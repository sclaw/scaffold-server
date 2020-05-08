{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Provider.QualificationBuilder.Create (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import qualified EdgeNode.Transport.Validator as Validator
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Qualification

import KatipController
import Katip
import Pretty
import Data.Traversable
import Data.Bifunctor
import Database.Transaction
import Control.Lens
import Data.Aeson.WithField
import Data.Generics.Product.Positions
import Data.Generics.Product.Fields
import Data.Maybe
import Data.Functor

controller
  :: (WithField
      "branch"
      (Id "branch")
      QualificationBuilder)
  -> KatipController (Response (Id "qualification"))
controller builder = do
  $(logTM) DebugS (logStr ("builder: " ++ mkPretty mempty builder))
  runTelegram builder
  let save = do
        hasql <- fmap (^.katipEnv.hasqlDbPool) ask
        katipTransaction hasql $ do
          let qualification =
                builder
                & position @2 %~
                  (fromJust .
                   (^.field @"qualificationBuilderQualification"))
          let clusters = builder^.position @2.field @"qualificationBuilderClusters"
          let fees = builder^.position @2.field @"qualificationBuilderFees"
          ident <- statement Provider.saveQualification qualification
          statement Provider.saveDependencies (ident, clusters)
          statement Provider.saveTuitionFees (ident, fees)
          return ident
  response <- fromValidation . (first (map asError)) <$> for (Validator.qualificationBuilder (builder^.position @2)) (const save)
  runTelegram response $> response