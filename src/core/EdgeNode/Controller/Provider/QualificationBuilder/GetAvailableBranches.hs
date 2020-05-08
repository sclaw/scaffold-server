{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.QualificationBuilder.GetAvailableBranches (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider

import Data.Aeson.WithField
import qualified Data.Text as T
import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Katip
import BuildInfo

controller :: Id "user" -> KatipController (Response [WithId (Id "branch") (OnlyField "title" T.Text)])
controller uid = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  runTelegram $location uid
  response <- katipTransaction hasql (statement Provider.getQualificationBuilderBranches uid)
  $(logTM) DebugS (logStr ("branches: " ++ mkPretty mempty response))
  runTelegram $location response
  return $ Ok $ response <&> \(ident, title) -> WithField ident (OnlyField title)