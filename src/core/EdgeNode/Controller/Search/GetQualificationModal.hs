{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module EdgeNode.Controller.Search.GetQualificationModal (controller) where

import EdgeNode.Transport.Response
import qualified EdgeNode.Statement.Search as Search
import EdgeNode.Transport.Search
import EdgeNode.Transport.Id

import Katip
import KatipController
import Data.Aeson.WithField.Extended
import Database.Transaction
import Control.Lens
import Pretty
import qualified Data.Text as T

controller :: Id "qualification" -> KatipController (Response (WithField "image" (Maybe (Id "file")) SearchQualificationModal))
controller qualification_id = do
  runTelegram qualification_id
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  response <- katipTransaction hasql $
    statement
    Search.getQualificationModal
    qualification_id
  $(logTM) DebugS (logStr ("resp: " ++ mkPretty mempty response))
  runTelegram response
  return $ liftMaybe @T.Text response "qualification not found"