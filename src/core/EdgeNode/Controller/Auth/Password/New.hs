module EdgeNode.Controller.Auth.Password.New (controller) where

import EdgeNode.Transport.Response

import Auth
import KatipController
import Data.Aeson.Unit

controller :: JWTUser -> KatipController (Response Unit)
controller _ = undefined