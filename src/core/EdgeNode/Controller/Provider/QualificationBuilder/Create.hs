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

import Auth
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
import BuildInfo
import qualified Data.Vector as V
import Protobuf.Scalar
import qualified Data.Text.Lazy as L

controller
  :: (WithField
      "branch"
      (Id "branch")
      QualificationBuilder)
  -> UserId
  -> KatipController (Response (Id "qualification"))
controller builder user_id = do
  $(logTM) DebugS (logStr ("builder: " ++ mkPretty mempty builder))
  runTelegram $location builder
  let save = do
        hasql <- fmap (^.katipEnv.hasqlDbPool) ask
        katipTransaction hasql $ do
          statement Provider.apiCaller ($location, user_id)
          let qualification =
                builder
                & position @2 %~
                  (fromJust .
                   (^.field @"qualificationBuilderQualification"))
          let clusters = builder^.position @2.field @"qualificationBuilderClusters"
          let fees = builder^.position @2.field @"qualificationBuilderFees"
          ident <- statement Provider.saveQualification qualification
          let xs =
                V.indexed clusters <&> \(idx, c) ->
                  ( fromIntegral idx
                  , (fmap (L.toStrict . stringValue) . clusterTitle) c)
          ids <- statement Provider.saveClusters (ident, xs)
          statement Provider.saveDependencies (ident, V.zip ids clusters)
          statement Provider.saveTuitionFees (ident, fees)
          return ident
  response <- fromValidation . (first (map asError)) <$> for (Validator.qualificationBuilder (builder^.position @2)) (const save)
  runTelegram $location response $> response