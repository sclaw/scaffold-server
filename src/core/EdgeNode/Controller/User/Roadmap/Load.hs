module EdgeNode.Controller.User.Roadmap.Load (controller) where

import EdgeNode.Transport.Response
import EdgeNode.Transport.User.Roadmap

import Auth
import KatipController

controller :: UserId -> KatipController (Response Roadmap)
controller _ = undefined