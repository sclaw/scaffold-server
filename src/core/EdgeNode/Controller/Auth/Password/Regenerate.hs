module EdgeNode.Controller.Auth.Password.Regenerate (controller) where

import EdgeNode.Transport.Auth
import EdgeNode.Transport.Response

import KatipController
import Data.Aeson.Unit

controller :: RegeneratePassword -> KatipController (Response Unit)
controller _ = undefined