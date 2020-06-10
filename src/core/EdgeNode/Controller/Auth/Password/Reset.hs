module EdgeNode.Controller.Auth.Password.Reset (controller) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response

import KatipController
import Data.Aeson.Unit

controller :: ResetPassword -> KatipController (Response Unit)
controller _ = undefined