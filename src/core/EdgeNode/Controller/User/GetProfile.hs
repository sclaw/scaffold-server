{-# LANGUAGE DataKinds #-}

module EdgeNode.Controller.User.GetProfile (controller) where

import EdgeNode.Transport.Id
import EdgeNode.Transport.Response
import EdgeNode.Transport.User

import Auth
import KatipController
import Data.Aeson.WithField

controller :: UserId -> KatipController (Response (WithId (Maybe (Id "image")) Profile))
controller _ = undefined