{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.PatchBranch (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id

import KatipController
import Data.Aeson.WithField
import Data.Aeson.Unit

controller :: Id -> WithField "files" [Id] (WithField "image" Id Branch) -> Id -> KatipController (Response Unit)
controller _ _ _ = undefined