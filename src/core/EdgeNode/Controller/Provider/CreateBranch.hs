{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.Provider.CreateBranch (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.Provider
import EdgeNode.Transport.Id

import KatipController
import Data.Aeson.WithField.Extended

controller :: OptField "files" [Id] (OptField "image" Id Branch) -> Id -> KatipController (Response Id)
controller _ _ = undefined