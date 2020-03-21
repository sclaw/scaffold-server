{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module EdgeNode.Controller.Provider.GetQualifications (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Id
import EdgeNode.Statement.Provider as Provider
import EdgeNode.Transport.Qualification

import Auth
import Katip
import KatipController
import Database.Transaction
import Control.Lens
import Pretty
import Data.Aeson.WithField

controller :: UserId -> KatipController (Response [WithId (Id "qualification") ListItem])
controller uid = do 
  hasql <- fmap (^.katipEnv.hasqlDbPool) ask 
  resp <- katipTransaction hasql (statement Provider.getQualifications uid)
  $(logTM) DebugS (logStr ("qualifications: " ++ mkPretty mempty resp))
  return $ Ok resp