module EdgeNode.Controller.Search (controller) where

import EdgeNode.Transport.Response

import KatipController
import qualified Data.Text as T

controller :: Maybe T.Text -> KatipController (Response T.Text)
controller _ = undefined