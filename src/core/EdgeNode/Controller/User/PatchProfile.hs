{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.User.PatchProfile (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User

import Auth
import KatipController
import Data.Aeson.WithField
import Data.Aeson.Unit

controller :: WithId (Maybe (Id "image")) Profile -> UserId -> KatipController (Response Unit)
controller _ _ = undefined