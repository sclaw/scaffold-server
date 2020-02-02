{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.GetBranches (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id

import KatipController
import Data.Aeson.WithField

controller :: Id -> KatipController (Response [(WithField "files" [Id] (WithField "image" Id Branch))])
controller _ = undefined