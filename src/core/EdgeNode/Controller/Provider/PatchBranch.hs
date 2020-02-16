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

controller :: Id "branch" -> WithField "files" [Id "file"] (WithField "image" (Id "img") Branch) -> Id "user" -> KatipController (Response Unit)
controller _ _ _ = undefined