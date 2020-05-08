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

controller :: Id "user" -> KatipController (Response [WithId (Id "branch") (OnlyField "title" T.Text)])
controller uid = do
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask
  runTelegram uid
  response <- katipTransaction hasql (statement Provider.getQualificationBuilderBranches uid)
  $(logTM) DebugS (logStr ("branches: " ++ mkPretty mempty response))
  runTelegram response
  return $ Ok $ response <&> \(ident, title) -> WithField ident (OnlyField title)